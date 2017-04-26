module VisualSearch exposing (..)

import GenGame
    exposing
        ( TrialResult(Continuing, Complete)
        , Reason(GoSuccess, WrongIndication, IndicationTimeout)
        , TrialFuns
        , checkTransition
        , updateReason
        )
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (src, class)
import Html.Events exposing (onClick)
import List.Extra
import Random exposing (Generator)
import Random.Extra
import Random.List
import Time exposing (Time)


type alias Trial =
    { correctPosition : Int
    , imageUrls : List String
    , stage : Stage
    , reason : Maybe Reason
    , selected : Maybe Int
    }


type Stage
    = NotStarted
    | FixationCross Time
    | SelectionGrid Time
    | SuccessAnimation Time
    | FailureAnimation Time


type alias Settings =
    { picturesPerTrial : Int
    , blockTrialCount : Int
    , fixationCross : Time
    , selectionGrid : Time
    , animation : Time
    }


init : Settings -> List String -> List String -> Generator (List (List Trial))
init settings responseUrls nonResponseUrls =
    Random.List.shuffle responseUrls
        |> Random.andThen
            (List.map
                (\rUrl ->
                    Random.map2
                        (\i nRUrls ->
                            nRUrls
                                |> List.take (settings.picturesPerTrial - 1)
                                |> List.Extra.splitAt i
                                |> (\( heads, tail ) ->
                                        initTrial i (heads ++ (rUrl :: tail))
                                   )
                        )
                        (Random.int 0 settings.picturesPerTrial)
                        (Random.List.shuffle nonResponseUrls)
                )
                >> Random.Extra.combine
                >> Random.map (List.Extra.greedyGroupsOf settings.blockTrialCount)
            )


initTrial : Int -> List String -> Trial
initTrial i urls =
    { correctPosition = i
    , imageUrls = urls
    , stage = NotStarted
    , reason = Nothing
    , selected = Nothing
    }


trialFuns : (Int -> msg) -> TrialFuns Settings Trial msg
trialFuns intMsg =
    { updateTime = updateTime
    , updateIndication = GenGame.defaultUpdateIndication
    , updateDirectionIndication = GenGame.defaultUpdateWithIndication
    , updateIntIndication = updateIndication
    , view = view intMsg
    }


updateTime : Settings -> Time -> Trial -> ( TrialResult Trial msg, Settings )
updateTime settings currTime trial =
    ( updateTimeHelper settings currTime trial, settings )


updateTimeHelper : Settings -> Time -> Trial -> TrialResult Trial msg
updateTimeHelper settings currTime trial =
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


updateIndication : Settings -> Time -> Int -> Trial -> ( TrialResult Trial msg, Settings )
updateIndication settings currTime selection trial =
    ( updateIndicationHelper currTime selection trial, settings )


updateIndicationHelper : Time -> Int -> Trial -> TrialResult Trial msg
updateIndicationHelper currTime selection trial =
    case trial.stage of
        SelectionGrid _ ->
            if selection == trial.correctPosition then
                Continuing
                    { trial
                        | stage = SuccessAnimation currTime
                        , reason = updateReason (GoSuccess currTime) trial.reason
                        , selected = Just selection
                    }
            else
                Continuing
                    { trial
                        | stage = FailureAnimation currTime
                        , reason =
                            updateReason
                                (WrongIndication currTime)
                                trial.reason
                        , selected = Just selection
                    }

        _ ->
            Continuing trial


view : (Int -> msg) -> Trial -> Html msg
view msgF trial =
    case trial.stage of
        NotStarted ->
            Html.p [ Html.Attributes.class "fixationCross has-text-centered" ] [ text "+" ]

        FixationCross _ ->
            div [ class "columns is-mobile" ]
                [ div [ class "column is-half is-offset-one-quarter" ]
                    [ Html.p [ Html.Attributes.class "fixationCross" ] [ text "+" ]
                    ]
                ]

        SelectionGrid _ ->
            div [ class "columns is-mobile" ]
                ((List.indexedMap
                    (\i url ->
                        img
                            [ Html.Attributes.class "vsImg"
                            , src url
                            , onClick (msgF i)
                            ]
                            []
                    )
                    trial.imageUrls
                 )
                    |> List.Extra.groupsOf 4
                    |> List.map (div [ class "column" ])
                )

        SuccessAnimation _ ->
            (animatedGrid trial)

        FailureAnimation _ ->
            (animatedGrid trial)


animatedGrid : Trial -> Html msg
animatedGrid trial =
    div [ class "columns is-mobile" ]
        ((List.indexedMap
            (\i url ->
                img
                    [ Html.Attributes.class
                        (selectClasses
                            i
                            trial.selected
                            trial.correctPosition
                        )
                    , src url
                    ]
                    []
            )
            trial.imageUrls
         )
            |> List.Extra.groupsOf 4
            |> List.map (div [ class "column" ])
        )


selectClasses : Int -> Maybe Int -> Int -> String
selectClasses index selected_ correct =
    case selected_ of
        Just selected ->
            if (index == selected) && (selected /= correct) then
                "vsImg red-shrink"
            else if index == correct then
                "vsImg green-grow"
            else
                "vsImg"

        Nothing ->
            "vsImg"
