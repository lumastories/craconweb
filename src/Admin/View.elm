module Admin.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (..)


adminPage : Model -> Html Msg
adminPage model =
    div []
        [ code []
            [ text <| "Welcome to the admin, " ++ model.jwtencoded
            ]
        ]
