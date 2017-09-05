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
        , logWithCondition
        , addIntervals
        , info
        , onIndication
        , timeoutFromSegmentStart
        , timeout
        , resultTimeout
        , startSession
        , trialFailed
        , isFailed
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
    -> ( Game msg, Random.Seed )
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
            gos ++ noGos ++ fillers |> List.take 3
    in
        trials
            |> Random.List.shuffle
            |> Random.andThen (addIntervals Nothing 500 0)
            |> Random.map
                (\shuffledTrials ->
                    (info infoString
                        :: startSession
                        :: log (BeginSession { seed = seedInt })
                        :: (shuffledTrials
                                ++ [ Game.Card.restart
                                        { gameDuration = gameDuration
                                        , nextTrials =
                                            trials
                                                |> Random.List.shuffle
                                                |> Random.andThen (Game.addIntervals Nothing 500 0)
                                                |> Random.andThen (Game.prependInterval Nothing 500 0)
                                        }
                                   ]
                           )
                    )
                        |> List.foldl (andThenCheckTimeout gameDuration) (Game.Card.complete (emptyState seedInt currentTime))
                )
            |> (\generator -> Random.step generator (Random.initialSeed seedInt))


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
        log BeginTrial { state | trialResult = Game.NoResult, trialStart = state.currTime, currentSeed = nextSeed }
            |> andThen (log (BeginDisplay bordered))
            |> andThen (log BeginInput)
            |> andThen
                (segment
                    [ onDirection goTrial direction
                    , resultTimeout (not goTrial) totalDuration
                    ]
                    bordered
                )
            |> andThen (logWithCondition isFailed (BeginDisplay redCross))
            |> andThen (segment [ trialFailed, timeoutFromSegmentStart redCrossDuration ] redCross)
            |> andThen (log EndTrial)
