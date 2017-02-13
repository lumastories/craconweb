module Main exposing (..)

import Model exposing (..)
import Update exposing (..)
import View exposing (..)
import Html exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = ( model, Cmd.none )
        , view = view
        , update = update
        , subscriptions = (\m -> Sub.none)
        }
