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
import Time
import Task


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
    , greeting : String
    , test : String
    , currentTime : Time.Time
    , currentTimeDelta : Time.Time
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
    , greeting = ""
    , test = ""
    , currentTime = 0
    , currentTimeDelta = 0
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
    | Tick Time.Time
    | SetGreeting Time.Time
    | VerifyToken Time.Time
    | SetTime Time.Time
    | SetDeltaTime Time.Time
    | GetTimeAndThen (Time.Time -> Msg)


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
        commands =
            [ Task.perform SetGreeting Time.now ]
    in
        ( initialModel flags location, Cmd.batch commands )
