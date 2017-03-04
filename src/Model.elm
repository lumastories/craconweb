module Model exposing (Model, Flags, Msg(..), init, initialModel)

{-| This is the Model, where we model and initialize our data.

# Data to bootstrap the app
@docs Flags

# How our application data should look
@docs Model

# Messages to pass throughout the application
@docs Msg


# Function that initializes the model with data
@docs init

# initial model data
@docs initialModel

-}

import Http
import Jwt
import Auth
import Navigation
import Routing exposing (..)
import Thing


{-| The data model for the entire application.

-}
type alias Model =
    { email : String
    , password : String
    , spin : Bool
    , activeRoute : Route
    , changes : Int
    , api : String
    , jwtencoded : String
    , jwtdecoded : Result Jwt.JwtError Auth.JwtPayload
    , error : String
    , presses : List Char
    , user : Thing.User
    , menuIsActive : Bool
    , mainMenuItems : List MenuItem
    , games : List Thing.Game
    }


{-| The data model as it is, was and will be

-}
initialModel : Flags -> Navigation.Location -> Model
initialModel flags location =
    { email = ""
    , password = ""
    , spin = False
    , activeRoute = parseLocation location
    , changes = 0
    , api = "http://localhost:8680"
    , jwtencoded = flags.token
    , jwtdecoded = Jwt.decodeToken Auth.decodeJwtPayload flags.token
    , error = ""
    , presses = []
    , user = Thing.initialUser flags.firstName
    , menuIsActive = False
    , mainMenuItems = initialMenuItems
    , games = Thing.initialGames
    }


{-| Represents messages to be passed throughout the application.
Typically called from the View and handled by the Update to move the Model forward

-}
type Msg
    = UpdateEmail String
    | UpdatePassword String
    | TryLogin
    | LoginResponse (Result Http.Error String)
    | UserResponse (Result Http.Error Thing.User)
    | Presses Char
    | UpdateLocation String
    | OnUpdateLocation Navigation.Location
    | MainMenuToggle
    | Logout


{-| Represents what data I should start up with
-}
type alias Flags =
    { token : String
    , firstName : String
    }


{-| initialize the model with data
-}
init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    let
        -- if token is valid, populate the user
        -- else Navigation.newUrl "/login"
        commands =
            [ Cmd.none ]

        --[ (Http.send UserResponse (getUser flags.sub))
        --]
    in
        ( initialModel flags location, Cmd.batch commands )
