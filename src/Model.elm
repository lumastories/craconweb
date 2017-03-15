module Model exposing (Model, Flags, Msg(..), init, initModel)

{-| This is the Model, where we model and initize our data.

# Data to bootstrap the app
@docs Flags

# How our application data should look
@docs Model

# Messages to pass throughout the application
@docs Msg


# Function that initizes the model with data
@docs init

# init model data
@docs initModel

-}

import Http
import Jwt
import Auth
import Navigation
import Routing exposing (..)
import Entity
import Time
import Genesis


{-| The data model for the entire application.

-}
type alias Model =
    { authRecord : Entity.AuthRecord
    , spin : Bool
    , activeRoute : Route
    , changes : Int
    , api : String
    , jwtencoded : String
    , jwtdecoded : Result Jwt.JwtError Auth.JwtPayload
    , error : String
    , presses : List Char
    , user : Entity.User
    , menuIsActive : Bool
    , mainMenuItems : List MenuItem
    , games : List Entity.Game
    , greeting : String
    , test : String
    , currentTime : Time.Time
    , currentTimeDelta : Time.Time
    , gimages : List Entity.Gimage
    , gameActions : List { time : Int, stimulus : Entity.Gameplay }
    }


{-| The data model as it is, was and will be

-}
initModel : Flags -> Navigation.Location -> Model
initModel flags location =
    { authRecord = Genesis.initAuthRecord
    , spin = False
    , activeRoute = parseLocation location
    , changes = 0
    , api = "http://localhost:8680"
    , jwtencoded = flags.token
    , jwtdecoded = Jwt.decodeToken Auth.decodeJwtPayload flags.token
    , error = ""
    , presses = []
    , user = Genesis.initUser
    , menuIsActive = False
    , mainMenuItems = initMenuItems
    , games = []
    , greeting = ""
    , test = ""
    , currentTime = 0
    , currentTimeDelta = 0
    , gimages = []
    , gameActions = []
    }


{-| Represents messages to be passed throughout the application.
Typically called from the View and handled by the Update to move the Model forward

-}
type Msg
    = UpdateEmail String
    | UpdatePassword String
    | TryLogin
    | LoginResponse (Result Http.Error Entity.Auth)
    | UserResponse (Result Http.Error Entity.User)
    | Presses Char
    | UpdateLocation String
    | OnUpdateLocation Navigation.Location
    | MainMenuToggle
    | Logout
    | GetTimeAndThen (Time.Time -> Msg)
    | CalcTimeDelta Time.Time
    | Tick Time.Time
    | VerifyToken Time.Time


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
            []
    in
        ( initModel flags location, Cmd.batch commands )
