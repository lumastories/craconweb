module Model exposing (Model, Msg(..), JwtPayload)

import Http
import Jwt
import Navigation
import Routing
import Entity
import Time


type alias Model =
    { authRecord : Entity.AuthRecord
    , spin : Bool
    , activeRoute : Routing.Route
    , changes : Int
    , api : String
    , jwtencoded : String
    , jwtdecoded : Result Jwt.JwtError JwtPayload
    , error : String
    , presses : List Char
    , user : Entity.User
    , menuIsActive : Bool
    , mainMenuItems : List Routing.MenuItem
    , greeting : String
    , test : String
    , currentTime : Time.Time
    , currentTimeDelta : Time.Time
    , games : List Entity.Game
    , gimages : List Entity.Gimage
    }


type alias JwtPayload =
    { aud : String
    , exp : Int
    , iat : Int
    , iss : String
    , sub : String
    , roles : List Entity.Role
    }


type Msg
    = UpdateEmail String
    | UpdatePassword String
    | TryLogin
    | LoginResponse (Result Http.Error Entity.Auth)
    | UserResponse (Result Http.Error Entity.User)
    | GameResponse (Result Http.Error Entity.Game)
    | GimageResponse (Result Http.Error Entity.Gimage)
    | Presses Char
    | UpdateLocation String
    | OnUpdateLocation Navigation.Location
    | MainMenuToggle
    | Logout
    | GetTimeAndThen (Time.Time -> Msg)
    | CalcTimeDelta Time.Time
    | Tick Time.Time
    | VerifyToken Time.Time
