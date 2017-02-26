module Model exposing (..)

import Navigation
import Http
import Jwt
import Auth


type alias Model =
    { email : String
    , password : String
    , history : List Navigation.Location
    , spin : Bool
    , page : Page
    , api : String
    , jwttoken : JwtToken
    , error : String
    , presses : List Char
    , payload : Result Jwt.JwtError Auth.JwtPayload
    }


type Msg
    = LoginEmail String
    | LoginPassword String
    | ChangePage Navigation.Location
    | LoginSend
    | LoginResponse (Result Http.Error JwtToken)
    | Presses Char


type Page
    = LoginPage
    | GamePage
    | BadgePage


type alias Flags =
    { token : String
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
            , spin = False
            , page = LoginPage
            , api = getApi location
            , jwttoken = JwtToken flags.token
            , error = ""
            , presses = []
            , payload = Jwt.decodeToken Auth.decodeJwtPayload flags.token
            }
    in
        ( model, Navigation.newUrl location.hash )


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
                "http://" ++ location.hostname ++ ":81"



--Debug.crash "Need API location for production enviornment"
