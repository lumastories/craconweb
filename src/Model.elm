module Model exposing (Model, User, Flags, JwtToken, Page(..), Msg(..), init)

{-| This is the Model, where we model and initialize our data.

# Data to bootstrap the app
@docs Flags

# JSON Web Token string to decode
@docs JwtToken

# How our application data should look
@docs Model

# Name, email, id of User
@docs User

# Messages to pass throughout the application
@docs Msg

# A page in the application
@docs Page

# Function that initializes the model with data
@docs init


-}

import Navigation
import Http
import Jwt
import Auth


{-| The data model for the entire application.

-}
type alias Model =
    { email : String
    , password : String
    , history : List Navigation.Location
    , spin : Bool
    , page : Page
    , api : String
    , jwttoken : JwtToken
    , jwtpayload : Result Jwt.JwtError Auth.JwtPayload
    , error : String
    , presses : List Char
    , user : User
    }


initialModel : Flags -> Navigation.Location -> Model
initialModel flags location =
    { email = ""
    , password = ""
    , history = [ location ]
    , spin = False
    , page = LoginPage
    , api = getApi location
    , jwttoken = JwtToken flags.token
    , jwtpayload = Jwt.decodeToken Auth.decodeJwtPayload flags.token
    , error = ""
    , presses = []
    , user = initialUser
    }


initialUser : User
initialUser =
    { id = ""
    , username = ""
    , email = ""
    , firstName = ""
    , lastName = ""
    }


{-| A user!
-}
type alias User =
    { id : String
    , email : String
    , username : String
    , firstName : String
    , lastName : String
    }


{-| Represents messages to be passed throughout the application.
Typically called from the View and handled by the Update to move the Model forward

-}
type Msg
    = LoginEmail String
    | LoginPassword String
    | ChangePage Navigation.Location
    | LoginSend
    | LoginResponse (Result Http.Error JwtToken)
    | UserResponse (Result Http.Error User)
    | Presses Char


{-| Represents where I am in the application
-}
type Page
    = LoginPage
    | GamePage
    | BadgePage


{-| Represents what data I should start up with
-}
type alias Flags =
    { token : String
    }


{-| Represents a JSON Web Token string to be decoded
-}
type alias JwtToken =
    { token : String
    }


{-| initialize the model with data
-}
init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    ( initialModel flags location, Navigation.newUrl location.hash )


{-| Converts the window.location into an API base. Currently only useful for
development and if the API is located on port 81 of the same server.

    getApi location == "http://localhost:8680"
-}
getApi : Navigation.Location -> String
getApi location =
    let
        logger =
            Debug.log (toString location) "getApi location"
    in
        case location.hostname of
            "localhost" ->
                "http://" ++ location.hostname ++ ":8680"

            "127.0.0.1" ->
                "http://" ++ location.hostname ++ ":8680"

            _ ->
                "http://" ++ location.hostname ++ ":81"
