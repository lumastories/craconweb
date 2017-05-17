module Game.Implementations.DotProbe exposing (init)

import Game.Card exposing (complete)
import Game
    exposing
        ( Game
        , Image
        , Layout(..)
        , BorderType(..)
        , LogEntry(..)
        , State
        , onDirection
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
        , leftOrRight
        )
import Random exposing (Generator)
import Random.List
import Time exposing (Time)


init :
    { fixationDuration : Time
    , imageDuration : Time
    , infoString : String
    , responseImages : List Image
    , nonResponseImages : List Image
    , seedInt : Int
    , currentTime : Time
    , gameDuration : Time
    }
    -> Game msg
init { fixationDuration, imageDuration, infoString, responseImages, nonResponseImages, seedInt, currentTime, gameDuration } =
    let
        trials =
            List.map2
                (\goImage noGoImage ->
                    trial
                        { fixationDuration = fixationDuration
                        , imageDuration = imageDuration
                        , goTrial = True
                        , gameDuration = gameDuration
                        , goImage = goImage
                        , noGoImage = noGoImage
                        }
                )
                responseImages
                nonResponseImages

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
                    (info infoString :: startSession :: log (BeginSession { seed = seedInt }) :: trials)
                        |> List.foldl (andThenCheckTimeout isTimeout) (Game.Card.complete (emptyState seedInt currentTime))
                )
            |> (\generator -> Random.step generator (Random.initialSeed seedInt))
            |> Tuple.first


trial :
    { fixationDuration : Time
    , imageDuration : Time
    , gameDuration : Time
    , goTrial : Bool
    , goImage : Image
    , noGoImage : Image
    }
    -> State
    -> Game msg
trial { fixationDuration, imageDuration, goTrial, gameDuration, goImage, noGoImage } state =
    let
        ( direction, firstSeed ) =
            Random.step Game.leftOrRight state.currentSeed

        probeDirectionGenerator =
            Random.float 0 1
                |> Random.map
                    (\n ->
                        if n < 0.9 then
                            direction
                        else
                            Game.flipDirection direction
                    )

        ( probeDirection, nextSeed ) =
            Random.step probeDirectionGenerator firstSeed

        borderless =
            None

        trial =
            case direction of
                Game.Left ->
                    Just (LeftRight borderless goImage noGoImage)

                Game.Right ->
                    Just (LeftRight borderless noGoImage goImage)

        fixation =
            Just (Fixation borderless)

        probe =
            Just (Probe borderless probeDirection)
    in
        log BeginTrial { state | trialResult = Game.NoResult, trialStart = state.currTime, currentSeed = nextSeed }
            |> andThen (log (BeginDisplay fixation))
            |> andThen (segment [ timeout fixationDuration ] fixation)
            |> andThen (log (BeginDisplay trial))
            |> andThen (segment [ timeout (fixationDuration + imageDuration) ] trial)
            |> andThen (log (BeginDisplay probe))
            |> andThen (log BeginInput)
            |> andThen (segment [ onDirection True direction ] probe)
            |> andThen (log EndTrial)
