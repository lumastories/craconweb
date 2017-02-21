port module Main exposing (..)

import Model exposing (..)
import Update exposing (..)
import View exposing (..)
import Navigation


{-

   On initialization we check localstorage for a JWT token
   If it exists we store it in our model and...
   Otherwise we store an empty string and...

-}


main : Program Flags Model Msg
main =
    Navigation.programWithFlags ChangePage
        { init = init
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }


port jwtAuth : String -> Cmd msg
