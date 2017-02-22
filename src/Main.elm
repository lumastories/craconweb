port module Main exposing (..)

import Model
import Update
import View
import Navigation
import Keyboard exposing (..)
import Char


{-

   On initialization we check localstorage for a JWT token
   If it exists we store it in our model and...
   Otherwise we store an empty string and...

-}


main : Program Model.Flags Model.Model Model.Msg
main =
    Navigation.programWithFlags Model.ChangePage
        { init = Model.init
        , view = View.view
        , update = Update.update
        , subscriptions = subscriptions
        }


subscriptions : Model.Model -> Sub Model.Msg
subscriptions model =
    Keyboard.presses (\code -> Model.Presses (Char.fromCode code))


port jwtAuth : String -> Cmd msg
