module Ui.Parts
    exposing
        ( notification
        , linkAttrs
        , notificationRemoteData
        , modal
        )

import Html exposing (..)
import Html.Attributes exposing (..)
import Model exposing (Msg(..))
import Html.Events exposing (onClick)
import Routing as R
import RemoteData
import Helpers


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


notificationRemoteData : RemoteData.WebData a -> Html Msg
notificationRemoteData remoteData =
    case remoteData of
        RemoteData.Loading ->
            notification (Just "Saving game data..") "is-info"

        RemoteData.Success _ ->
            notification (Just "Game data saved") "is-info"

        RemoteData.NotAsked ->
            text ""

        RemoteData.Failure error ->
            notification (Just <| Helpers.httpHumanError error) "is-danger"


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
