module Main exposing (..)

import Model
import Update
import View
import Keyboard exposing (..)
import Char
import RouteUrl
import Router exposing (..)


{-

   On initialization we check localstorage for a JWT token
   If it exists we store it in our model and...
   Otherwise we store an empty string and...

-}


main : RouteUrl.RouteUrlProgram Model.Flags Model.Model Model.Msg
main =
    RouteUrl.programWithFlags
        { delta2url = delta2url
        , location2messages = location2messages
        , init = Model.init
        , view = View.view
        , update = Update.update
        , subscriptions = subscriptions
        }


subscriptions : Model.Model -> Sub Model.Msg
subscriptions model =
    Keyboard.presses (\code -> Model.Presses (Char.fromCode code))
