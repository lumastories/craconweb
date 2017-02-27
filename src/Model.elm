module Model exposing (Model, User, Flags, Page(..), Msg(..), init)

{-| This is the Model, where we model and initialize our data.

# Data to bootstrap the app
@docs Flags

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

import Http
import Jwt
import Auth
import Time exposing (Time)
import Task


{-| The data model for the entire application.

-}
type alias Model =
    { email : String
    , password : String
    , spin : Bool
    , activePage : Page
    , api : String
    , jwtencoded : String
    , jwtdecoded : Result Jwt.JwtError Auth.JwtPayload
    , error : String
    , presses : List Char
    , user : User
    }


initialModel : Flags -> Model
initialModel flags =
    { email = ""
    , password = ""
    , spin = False
    , activePage = Login
    , api = "http://localhost:8680"
    , jwtencoded = flags.token
    , jwtdecoded = Jwt.decodeToken Auth.decodeJwtPayload flags.token
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
    | TryLogin
    | LoginResponse (Result Http.Error String)
    | UserResponse (Result Http.Error User)
    | Presses Char
    | SetActivePage Page
    | CheckTokenExpiry Time


{-| Represents where I am in the application
-}
type Page
    = AccessDenied
    | PageNotFound
    | Login
    | Games
    | Badges


{-| Represents what data I should start up with
-}
type alias Flags =
    { token : String
    }


{-| initialize the model with data
-}
init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initialModel flags, Task.perform CheckTokenExpiry Time.now )
