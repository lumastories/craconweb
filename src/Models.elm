module Models exposing (..)

import Material


type alias Model =
    { email : String
    , password : String
    , mdl :
        Material.Model
    }


type alias Mdl =
    Material.Model


model : Model
model =
    { email = ""
    , password = ""
    , mdl =
        Material.model
    }
