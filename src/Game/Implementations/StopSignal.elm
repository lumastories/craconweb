module Game.Implementations.StopSignal
    exposing
        ( init
        )

import Game
    exposing
        ( BorderType(..)
        , Game
        , Image
        , Layout(..)
        , LogEntry(..)
        , State
        , andThen
        , andThenCheckTimeout
        , emptyState
        , info
        , isFailed
        , leftOrRight
        , log
        , logWithCondition
        , onDirection
        , onIndication
        , resultTimeout
        , segment
        , startSession
        , timeout
        , trialFailed
        )
import Game.Card exposing (complete)
import Random exposing (Generator)
import Random.List
import Time exposing (Time)


init :
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
init { borderDelay, totalDuration, infoString, responseImages, nonResponseImages, seedInt, currentTime, gameDuration, redCrossDuration } =
    let
        gos =
            responseImages
                |> List.map
                    (trial
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
                    (trial
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

        addIntervals =
            Game.addIntervals Nothing (500 * Time.millisecond) 0
    in
        Random.List.shuffle trials
            |> Random.andThen addIntervals
            |> Random.map
                (\trials ->
                    (info infoString :: startSession :: log (BeginSession { seed = seedInt }) :: trials)
                        |> List.foldl (andThenCheckTimeout isTimeout) (Game.Card.complete (emptyState seedInt currentTime))
                )
            |> (\generator -> Random.step generator (Random.initialSeed seedInt))
            |> Tuple.first


trial :
    { borderDelay : Time
    , totalDuration : Time
    , gameDuration : Time
    , redCrossDuration : Time
    , goTrial : Bool
    }
    -> Image
    -> State
    -> Game msg
trial { borderDelay, totalDuration, goTrial, gameDuration, redCrossDuration } image state =
    let
        borderType =
            if goTrial then
                Blue
            else
                Gray

        borderless =
            Just (Single None image)

        bordered =
            Just (Single borderType image)

        redCross =
            Just (RedCross borderType)
    in
        log BeginTrial { state | trialResult = Game.NoResult, trialStart = state.currTime }
            |> andThen (log (BeginDisplay borderless))
            |> andThen (segment [ timeout borderDelay ] borderless)
            |> andThen (log BeginInput)
            |> andThen (log (BeginDisplay bordered))
            |> andThen (segment [ onIndication goTrial, resultTimeout (not goTrial) totalDuration ] bordered)
            |> andThen (logWithCondition isFailed (BeginDisplay redCross))
            |> andThen (segment [ trialFailed, timeout (totalDuration + redCrossDuration) ] redCross)
            |> andThen (log EndTrial)
