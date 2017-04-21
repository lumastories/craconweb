module DotProbe exposing (..)

import GenGame
    exposing
        ( Direction(Left, Right)
        , TrialResult(Continuing, Complete)
        , Reason(GoSuccess, WrongIndication)
        , TrialFuns
        , checkTransition
        , wrapper
        )
import List.Extra
import Html exposing (Html, text, div, img)
import Html.Attributes exposing (src, class)
import Svg exposing (Svg, svg, circle)
import Svg.Attributes exposing (cx, cy, r)
import Random exposing (Generator)
import Random.Extra
import Random.List
import Time exposing (Time)


type alias Trial =
    { probePosition : Direction
    , leftImageUrl : String
    , rightImageUrl : String
    , stage : Stage
    , reason : Maybe Reason
    }


type Stage
    = NotStarted
    | FixationCross Time
    | Pictures Time
    | Probe Time


type alias Settings =
    { blockCount : Int
    , pictures : Time
    , fixationCross : Time
    }


init : Settings -> List String -> List String -> Generator (List (List Trial))
init settings responseUrls nonResponseUrls =
    Random.Extra.andThen2
        (\sGo sNoGo ->
            let
                directions dLeft dRight ( l, r ) =
                    nintyTenSplit dLeft dRight
                        |> Random.map (initTrial l r)

                triple =
                    List.concat << List.repeat 3

                leftGos =
                    List.Extra.zip sGo sNoGo
                        |> triple
                        |> List.map (directions Left Right)

                rightGos =
                    List.Extra.zip sNoGo sGo
                        |> triple
                        |> List.map (directions Right Left)
            in
                Random.Extra.combine (leftGos ++ rightGos)
                    |> Random.andThen Random.List.shuffle
                    |> Random.map (List.Extra.greedyGroupsOf settings.blockCount)
        )
        (Random.List.shuffle responseUrls)
        (Random.List.shuffle nonResponseUrls)


initTrial : String -> String -> Direction -> Trial
initTrial left right direction =
    { probePosition = direction
    , leftImageUrl = left
    , rightImageUrl = right
    , stage = NotStarted
    , reason = Nothing
    }


trialFuns : TrialFuns Settings Trial msg
trialFuns =
    { updateTime = updateTime
    , updateIndication = GenGame.defaultUpdateIndication
    , updateDirectionIndication = updateIndication
    , updateIntIndication = GenGame.defaultUpdateWithIndication
    , view = view
    }


nintyTenSplit : a -> a -> Generator a
nintyTenSplit ninty ten =
    Random.Extra.frequency
        [ ( 0.9, Random.Extra.constant ninty )
        , ( 0.1, Random.Extra.constant ten )
        ]


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
                trans settings.fixationCross timeSince (Continuing { trial | stage = Pictures currTime })

            Pictures timeSince ->
                trans settings.pictures timeSince (Continuing { trial | stage = Probe currTime })

            Probe timeSince ->
                Continuing trial


updateIndication : Settings -> Time -> Direction -> Trial -> ( TrialResult Trial msg, Settings )
updateIndication settings currTime direction trial =
    ( updateIndicationHelper currTime direction trial, settings )


updateIndicationHelper : Time -> Direction -> Trial -> TrialResult Trial msg
updateIndicationHelper currTime direction trial =
    case trial.stage of
        Probe timeSince ->
            if trial.probePosition == direction then
                Complete (Just (GoSuccess currTime))
            else
                Complete (Just (WrongIndication currTime))

        _ ->
            Continuing trial



--probePosition
--
--rightImageUrl


dot : Direction -> Svg msg
dot dir =
    case dir of
        Left ->
            div [ class "probeRight" ] [ svg [] [ circle [ cx "10", cy "10", r "10" ] [] ] ]

        Right ->
            div [ class "probeLeft" ] [ svg [] [ circle [ cx "10", cy "10", r "10" ] [] ] ]


view : Trial -> Html msg
view trial =
    case trial.stage of
        NotStarted ->
            text ""

        FixationCross _ ->
            div
                [ class "columns is-mobile" ]
                [ div
                    [ class "column" ]
                    [ div [ class "fixationCross" ] [ text "+" ] ]
                ]

        Pictures _ ->
            wrapper
                [ img [ class "squeezed", src trial.leftImageUrl ] []
                , img [ class "is-pulled-right squeezed", src trial.rightImageUrl ] []
                ]

        Probe _ ->
            wrapper
                [ dot trial.probePosition ]
