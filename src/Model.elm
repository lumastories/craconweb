module Model exposing (..)
import Material
import Navigation


type alias Model =
    { email : String
    , password : String
    , history : List Navigation.Location
    , mdl : Material.Model
    , spin : Bool
    , page : Page
    }


type alias Mdl =
    Material.Model

type Page
    = LoginPage
    | GamePage
    | BadgePage


type Msg
    = UpdateEmail String
    | UpdatePassword String
    | Mdl (Material.Msg Msg)
    | ChangePage Navigation.Location
    | Noop
