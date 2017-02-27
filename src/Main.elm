module Main exposing (..)

import Model
import Update
import View
import Keyboard exposing (..)
import Char
import Navigation


main : Program Model.Flags Model.Model Model.Msg
main =
    Navigation.programWithFlags Model.OnLocationChange
        { init = Model.init
        , view = View.view
        , update = Update.update
        , subscriptions = subscriptions
        }


subscriptions : Model.Model -> Sub Model.Msg
subscriptions model =
    Keyboard.presses (\code -> Model.Presses (Char.fromCode code))
