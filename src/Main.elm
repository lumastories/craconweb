module Main exposing (..)

import Model exposing (..)
import Update exposing (..)
import View exposing (..)
import Navigation
import Material

main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = (\m -> Sub.none)
        }


model : Navigation.Location -> Model
model location =
    { email = ""
    , password = ""
    , history = [ location ]
    , mdl =
        Material.model
    , spin = False
    }

init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( model location, Cmd.none )
