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
    , reason : Maybe Reason
    }


type Stage
    = NotStarted
    | FixationCross Time
    | SelectionGrid Time
    | SuccessAnimation Time
    | FailureAnimation Time


type alias Settings =
    { fixationCross : Time
    , selectionGrid : Time
    , animation : Time
    }


updateTime : Settings -> Time -> Trial -> TrialResult Trial msg
updateTime settings currTime trial =
    let
        trans =
            checkTransition trial currTime
    in
        case trial.stage of
            NotStarted ->
                Continuing { trial | stage = FixationCross currTime }

            FixationCross timeSince ->
                trans timeSince
                    settings.fixationCross
                    (Continuing { trial | stage = SelectionGrid currTime })

            SelectionGrid timeSince ->
                trans timeSince
                    settings.selectionGrid
                    (Continuing
                        { trial
                            | stage = FailureAnimation currTime
                            , reason = updateReason IndicationTimeout trial.reason
                        }
                    )

            SuccessAnimation timeSince ->
                trans timeSince
                    settings.animation
                    (Complete trial.reason)

            FailureAnimation timeSince ->
                trans timeSince
                    settings.animation
                    (Complete trial.reason)


updateIndication : Time -> Int -> Trial -> TrialResult Trial msg
updateIndication currTime selection trial =
    case trial.stage of
        SelectionGrid _ ->
            if selection == trial.correctPosition then
                Continuing
                    { trial
                        | stage = SuccessAnimation currTime
                        , reason = updateReason (GoSuccess currTime) trial.reason
                    }
            else
                Continuing
                    { trial
                        | stage = FailureAnimation currTime
                        , reason =
                            updateReason
                                (WrongIndication currTime)
                                trial.reason
                    }

        _ ->
            Continuing trial


view : (Int -> msg) -> Trial -> Html msg
view msgF trial =
    case trial.stage of
        NotStarted ->
            img [ src "fixationCrossUrl" ] []

        FixationCross _ ->
            img [ src "fixationCrossUrl" ] []

        SelectionGrid _ ->
            div []
                (List.indexedMap (\i url -> img [ src url, onClick (msgF i) ] []) trial.imageUrls)

        SuccessAnimation _ ->
            text ""

        FailureAnimation _ ->
            text ""
