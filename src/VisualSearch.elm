module VisualSearch exposing (..)

import GenGame
    exposing
        ( TrialResult(Continuing, Complete)
        , Reason(GoSuccess, WrongIndication, IndicationTimeout)
        , checkTransition
        , updateReason
        )
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Time exposing (Time)


type alias Trial =
    { correctPosition : Int
    , imageUrls : List String
    , stage : Stage
    , lastTransition : Time
    , reason : Maybe Reason
    }


type Stage
    = FixationCross
    | SelectionGrid
    | SuccessAnimation
    | FailureAnimation


type alias Settings =
    { fixationCross : Time
    , selectionGrid : Time
    , animation : Time
    }


updateTime : Settings -> Time -> Trial -> TrialResult Trial msg
updateTime settings currTime trial =
    let
        trans =
            checkTransition trial currTime trial.lastTransition
    in
        case trial.stage of
            FixationCross ->
                trans settings.fixationCross
                    (Continuing { trial | stage = SelectionGrid })

            SelectionGrid ->
                trans settings.selectionGrid
                    (Continuing
                        { trial
                            | stage = FailureAnimation
                            , reason = updateReason IndicationTimeout trial.reason
                        }
                    )

            SuccessAnimation ->
                trans settings.animation
                    (Complete trial.reason)

            FailureAnimation ->
                trans settings.animation
                    (Complete trial.reason)


updateIndication : Time -> Int -> Trial -> TrialResult Trial msg
updateIndication currTime selection trial =
    if trial.stage == SelectionGrid then
        if selection == trial.correctPosition then
            Continuing
                { trial
                    | stage = SuccessAnimation
                    , reason = updateReason (GoSuccess currTime) trial.reason
                }
        else
            Continuing
                { trial
                    | stage = FailureAnimation
                    , reason =
                        updateReason
                            (WrongIndication currTime)
                            trial.reason
                }
    else
        Continuing trial


view : (Int -> msg) -> Trial -> Html msg
view msgF trial =
    case trial.stage of
        FixationCross ->
            img [ src "fixationCrossUrl" ] []

        SelectionGrid ->
            div []
                (List.indexedMap (\i url -> img [ src url, onClick (msgF i) ] []) trial.imageUrls)

        SuccessAnimation ->
            text ""

        FailureAnimation ->
            text ""
