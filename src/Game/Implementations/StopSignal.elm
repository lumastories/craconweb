module Game.Implementations.StopSignal
    exposing
        ( init
        )

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
        , info
        , onIndication
        , timeout
        , resultTimeout
        , startSession
        , isFailed
        , trialFailed
        , leftOrRight
        , onDirection
        )
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
    , fmri : Game.Fmri
    }
    -> Game msg
init { borderDelay, totalDuration, infoString, responseImages, nonResponseImages, seedInt, currentTime, gameDuration, redCrossDuration, fmri } =
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

        addRests =
            (case fmri of
                Game.YesFmri _ ->
                    Game.addRests Nothing (3 * Time.second) (4 * Time.second)

                Game.NotFmri ->
                    Game.addRests Nothing (500 * Time.millisecond) 0
            )
    in
        Random.List.shuffle trials
            |> Random.andThen addRests
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
                Grey

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
