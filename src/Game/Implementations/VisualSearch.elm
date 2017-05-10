module Game.Implementations.VisualSearch exposing (init)

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
        , timeout
        , timeoutFromSegmentStart
        , selectTimeout
        , startSession
        , leftOrRight
        , onSelect
        , showZoom
        )
import Random exposing (Generator)
import Random.List
import Time exposing (Time)
import List.Extra


init :
    { fixationDuration : Time
    , imageDuration : Time
    , infoString : String
    , responseImages : List Image
    , nonResponseImages : List Image
    , seedInt : Int
    , currentTime : Time
    , gameDuration : Time
    , zoomDuration : Time
    }
    -> Game msg
init { fixationDuration, imageDuration, zoomDuration, infoString, responseImages, nonResponseImages, seedInt, currentTime, gameDuration } =
    let
        trials =
            responseImages
                |> List.map
                    (trial
                        { fixationDuration = fixationDuration
                        , imageDuration = imageDuration
                        , zoomDuration = zoomDuration
                        , goTrial = True
                        , gameDuration = gameDuration
                        , noGoImages = nonResponseImages
                        }
                    )

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
    , zoomDuration : Time
    , gameDuration : Time
    , goTrial : Bool
    , noGoImages : List Image
    }
    -> Image
    -> State
    -> Game msg
trial { fixationDuration, imageDuration, zoomDuration, goTrial, gameDuration, noGoImages } goImage state =
    let
        ( noGoImagesShuffled, newSeed ) =
            Random.step (Random.List.shuffle noGoImages) state.currentSeed

        ( images, nextSeed ) =
            Random.step (Random.List.shuffle (goImage :: (List.take 15 noGoImagesShuffled))) newSeed

        goIndex =
            case List.Extra.elemIndex goImage images of
                Just index ->
                    index

                Nothing ->
                    Debug.crash "goImage is not in the list for some reason"

        trial =
            Just (SelectGrid None { columns = 4, images = images, goIndex = goIndex })

        fixation =
            Just (Fixation None)
    in
        log BeginTrial { state | trialResult = Game.NoResult, trialStart = state.currTime, currentSeed = nextSeed }
            |> andThen (log (BeginDisplay fixation))
            |> andThen (segment [ timeout fixationDuration ] fixation)
            |> andThen (log (BeginDisplay trial))
            |> andThen (log BeginInput)
            |> andThen (segment [ onSelect goIndex, selectTimeout (fixationDuration + imageDuration) ] trial)
            |> andThen (segment [ timeoutFromSegmentStart zoomDuration ] trial)
            |> andThen (log EndTrial)
