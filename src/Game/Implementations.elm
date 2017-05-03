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
        , andThenCheckTimeout
        , emptyState
        , segment
        , log
        , addRests
        , info
        , onIndication
        , timeout
        , resultTimeout
        , startSession
        , showRedCross
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
    , redCrossDuration : Time
    }
    -> Game msg
stopSignalInit { borderDelay, totalDuration, infoString, responseImages, nonResponseImages, seedInt, currentTime, gameDuration, redCrossDuration } =
    let
        gos =
            responseImages
                |> List.map
                    (stopSignalTrial
                        { borderDelay = borderDelay
                        , totalDuration = totalDuration
                        , goTrial = True
                        , gameDuration = gameDuration
                        , redCrossDuration = redCrossDuration
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
                        , redCrossDuration = redCrossDuration
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
                        |> List.foldl (andThenCheckTimeout isTimeout) (Game.Card.complete (emptyState currentTime))
                )
            |> (\generator -> Random.step generator (Random.initialSeed seedInt))
            |> Tuple.first


stopSignalTrial :
    { borderDelay : Time
    , totalDuration : Time
    , gameDuration : Time
    , redCrossDuration : Time
    , goTrial : Bool
    }
    -> Image
    -> State
    -> Game msg
stopSignalTrial { borderDelay, totalDuration, goTrial, gameDuration, redCrossDuration } image state =
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

        redCross =
            Just (RedCross borderType)
    in
        complete { state | trialResult = Nothing, trialStart = state.currTime }
            |> andThen (log (BeginDisplay borderless))
            |> andThen (segment [ onIndication False, timeout borderDelay ] borderless)
            |> andThen (log (BeginDisplay bordered))
            |> andThen
                (segment
                    [ onIndication goTrial
                    , resultTimeout (not goTrial) totalDuration
                    ]
                    bordered
                )
            |> andThen (segment [ showRedCross, timeout (totalDuration + redCrossDuration) ] redCross)
