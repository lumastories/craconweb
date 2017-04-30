module Game.Implementations exposing (..)

import Game.Card exposing (complete)
import Game
    exposing
        ( Game
        , Image
        , Layout(..)
        , BorderType(..)
        , LogEntry(..)
        , State
        , andThen
        , emptyState
        , segment
        , log
        , addRests
        , info
        , onIndication
        , timeout
        , resultTimeout
        , startSession
        )
import Random exposing (Generator)
import Random.List
import Time exposing (Time)


stopSignalInit :
    { borderDelay : Time
    , totalDuration : Time
    , infoString : String
    , responseImages : List Image
    , nonResponseImages : List Image
    , seedInt : Int
    , currentTime : Time
    , gameDuration : Time
    }
    -> Game msg
stopSignalInit { borderDelay, totalDuration, infoString, responseImages, nonResponseImages, seedInt, currentTime, gameDuration } =
    let
        gos =
            responseImages
                |> List.map
                    (stopSignalTrial
                        { borderDelay = borderDelay
                        , totalDuration = totalDuration
                        , goTrial = True
                        , gameDuration = gameDuration
                        }
                    )

        noGos =
            nonResponseImages
                |> List.map
                    (stopSignalTrial
                        { borderDelay = borderDelay
                        , totalDuration = totalDuration
                        , goTrial = False
                        , gameDuration = gameDuration
                        }
                    )

        trials =
            gos ++ noGos

        isTimeout state =
            state.sessionStart
                |> Maybe.map (\sessionStart -> sessionStart + gameDuration < state.currTime)
                |> Maybe.withDefault False
    in
        Random.List.shuffle trials
            |> Random.andThen (addRests Nothing 500 0)
            |> Random.map
                (\trials ->
                    (info infoString :: startSession :: log (BeginSession seedInt) :: trials)
                        |> List.foldl (Game.andThen isTimeout) (Game.Card.complete (emptyState currentTime))
                )
            |> (\generator -> Random.step generator (Random.initialSeed seedInt))
            |> Tuple.first


stopSignalTrial :
    { borderDelay : Time
    , totalDuration : Time
    , gameDuration : Time
    , goTrial : Bool
    }
    -> Image
    -> State
    -> Game msg
stopSignalTrial { borderDelay, totalDuration, goTrial, gameDuration } image state =
    let
        borderType =
            if goTrial then
                Blue
            else
                Grey

        borderless =
            Just (Single None image)

        bordered =
            Just (Single borderType image)
    in
        complete { state | trialResult = Nothing, trialStart = state.currTime }
            |> andThen (always False) (log (BeginDisplay borderless))
            |> andThen (always False) (segment [ onIndication False, timeout borderDelay ] borderless)
            |> andThen (always False) (log (BeginDisplay bordered))
            |> andThen (always False)
                (segment
                    [ onIndication goTrial
                    , resultTimeout (not goTrial) totalDuration
                    ]
                    bordered
                )
            |> andThen (always False) (segment [ timeout totalDuration ] bordered)
