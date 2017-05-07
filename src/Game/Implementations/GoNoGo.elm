module Game.Implementations.GoNoGo exposing (init)

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
        , leftOrRight
        , onDirection
        )
import Random exposing (Generator)
import Random.List
import Time exposing (Time)


init :
    { totalDuration : Time
    , infoString : String
    , responseImages : List Image
    , nonResponseImages : List Image
    , fillerImages : List Image
    , seedInt : Int
    , currentTime : Time
    , gameDuration : Time
    , redCrossDuration : Time
    }
    -> Game msg
init { totalDuration, infoString, responseImages, nonResponseImages, fillerImages, seedInt, currentTime, gameDuration, redCrossDuration } =
    let
        gos =
            responseImages
                |> List.map
                    (trial
                        { totalDuration = totalDuration
                        , goTrial = True
                        , gameDuration = gameDuration
                        , redCrossDuration = redCrossDuration
                        }
                    )

        noGos =
            nonResponseImages
                |> List.map
                    (trial
                        { totalDuration = totalDuration
                        , goTrial = False
                        , gameDuration = gameDuration
                        , redCrossDuration = redCrossDuration
                        }
                    )

        fillers =
            fillerImages
                |> List.map
                    (trial
                        { totalDuration = totalDuration
                        , goTrial = True
                        , gameDuration = gameDuration
                        , redCrossDuration = redCrossDuration
                        }
                    )

        trials =
            gos ++ noGos ++ fillers

        isTimeout state =
            state.sessionStart
                |> Maybe.map (\sessionStart -> sessionStart + gameDuration < state.currTime)
                |> Maybe.withDefault False
    in
        trials
            |> Random.List.shuffle
            |> Random.andThen (addRests Nothing 500 0)
            |> Random.map
                (\trials ->
                    (info infoString :: startSession :: log (BeginSession seedInt) :: trials)
                        |> List.foldl (andThenCheckTimeout isTimeout) (Game.Card.complete (emptyState seedInt currentTime))
                )
            |> (\generator -> Random.step generator (Random.initialSeed seedInt))
            |> Tuple.first


trial :
    { totalDuration : Time
    , gameDuration : Time
    , redCrossDuration : Time
    , goTrial : Bool
    }
    -> Image
    -> State
    -> Game msg
trial { totalDuration, goTrial, gameDuration, redCrossDuration } image state =
    let
        ( direction, nextSeed ) =
            Random.step Game.leftOrRight state.currentSeed

        borderType =
            if goTrial then
                Black
            else
                Dashed

        bordered =
            Just (LeftOrRight borderType direction image)

        redCross =
            Just (RedCross borderType)
    in
        log BeginTrial { state | trialResult = Nothing, trialStart = state.currTime, currentSeed = nextSeed }
            |> andThen (log (BeginDisplay bordered))
            |> andThen
                (segment
                    [ onDirection goTrial direction
                    , resultTimeout (not goTrial) totalDuration
                    ]
                    bordered
                )
            |> andThen (segment [ showRedCross, timeout (totalDuration + redCrossDuration) ] redCross)
            |> andThen (log EndTrial)
