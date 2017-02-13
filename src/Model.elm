module Model exposing (..)

import Material
import Navigation


type alias Model =
    { email : String
    , password : String
    , history : List Navigation.Location
    , mdl :
        Material.Model
    , spin : Bool
    }


type alias Mdl =
    Material.Model


model : Navigation.Location -> Model
model location =
    { email = ""
    , password = ""
    , history = [ location ]
    , mdl =
        Material.model
    , spin = True
    }
