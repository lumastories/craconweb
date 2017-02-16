module Main exposing (..)

import Model exposing (..)
import Update exposing (..)
import View exposing (..)
import Navigation
import Material


model : Navigation.Location -> Model
model location =
    { email = ""
    , password = ""
    , history = [ location ]
    , mdl =
        Material.model
    , spin = False
    , page = LoginPage
    }


main : Program Never Model Msg
main =
    Navigation.program ChangePage
        { init = init
        , view = view
        , update = update
        , subscriptions = (\m -> Sub.none)
        }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( model location, Cmd.none )
