module Ui.Parts exposing (notification, linkAttrs, modal)

import Html exposing (..)
import Html.Attributes exposing (..)
import Model exposing (Msg(..))
import Html.Events exposing (onClick)
import Routing as R


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


linkAttrs : String -> List (Attribute Msg)
linkAttrs path =
    [ href <| path, R.onLinkClick <| UpdateLocation path ]


modal : List (Html msg) -> Html msg
modal children =
    div
        [ class <| "modal is-active" ]
        [ div
            [ class "modal-background" ]
            []
        , div
            [ class "modal-content" ]
            children
        ]
