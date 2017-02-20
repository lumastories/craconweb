module Main exposing (..)

import Model exposing (..)
import Update exposing (..)
import View exposing (..)

import Navigation


main : Program Never Model Msg
main =
    Navigation.program ChangePage
        { init = init
        , view = view
        , update = update
        , subscriptions = (\model -> Sub.none)
        }


