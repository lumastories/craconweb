module Model exposing (..)

import Material
import Navigation
import Http


type alias Model =
    { email : String
    , password : String
    , history : List Navigation.Location
    , mdl : Material.Model
    , spin : Bool
    , page : Page
    , api : String
    , jwttoken : JwtToken
    , error : String
    , presses : List Char
    }


type Msg
    = UpdateEmail String
    | UpdatePassword String
    | Mdl (Material.Msg Msg)
    | ChangePage Navigation.Location
    | Login
    | LoginResponse (Result Http.Error JwtToken)
    | Presses Char


type Page
    = LoginPage
    | GamePage
    | BadgePage


type alias Flags =
    { user : String
    , token : String
    }


type alias JwtToken =
    { token : String
    }


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
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
            , jwttoken = JwtToken flags.token
            , error = ""
            , presses = []
            }
    in
        ( model, Cmd.none )


getApi : Navigation.Location -> String
getApi location =
    let
        logger =
            Debug.log (toString location) "getApi location"
    in
        case location.hostname of
            "localhost" ->
                "http://" ++ location.hostname ++ ":8680"

            -- dev api
            "127.0.0.1" ->
                "http://" ++ location.hostname ++ ":8680"

            -- dev api
            _ ->
                --location.protocol ++ "//" ++ location.hostname ++ "/api"
                Debug.crash "Need API location for production enviornment"
