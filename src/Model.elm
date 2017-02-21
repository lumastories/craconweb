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
    , api : String
    , token : String
    }

type Msg
    = UpdateEmail String
    | UpdatePassword String
    | Mdl (Material.Msg Msg)
    | ChangePage Navigation.Location

type Page
    = LoginPage
    | GamePage
    | BadgePage

type alias Flags =
  { user : String
  , token : String
  }

init : Flags -> Navigation.Location -> (Model, Cmd Msg)
init flags location =
    let
        model =
            { email = ""
            , password = ""
            , history = [ location ]
            , mdl =
                Material.model
            , spin = False
            , page = LoginPage
            , api = getApi location
            , token = flags.token
            }
    in
        ( model, Cmd.none )


getApi : Navigation.Location -> String
getApi location = 
    let
        logger =
            Debug.log (toString location) 1
    in
        case location.protocol of
            "file:" ->
                "http://127.0.0.1:8680" -- dev api
            _ ->
                --location.protocol ++ "//" ++ location.hostname ++ "/api"
                Debug.crash "Need API location for production enviornment"