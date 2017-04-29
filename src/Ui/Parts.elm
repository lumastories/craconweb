module Ui.Parts exposing (notification)

import Html exposing (..)
import Html.Attributes exposing (..)
import Model exposing (Msg(..))
import Html.Events exposing (onClick)


notification : Maybe String -> String -> Html Msg
notification notifText mods =
    case notifText of
        Just nTxt ->
            div
                [ class <| "notification " ++ mods ]
                [ button [ class "delete", onClick ResetNotifications ] []
                , text nTxt
                ]

        Nothing ->
            div [] []
