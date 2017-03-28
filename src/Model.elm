module Model exposing (..)

import Http
import Jwt
import Navigation
import Routing
import Entity
import Time
import Api


type alias Model =
    { api : String
    , jwtencoded : String
    , spin : Bool
    , activeRoute : Routing.Route
    , error : String
    , presses : List Char
    , visitor : Visitor
    , menuIsActive : Bool
    , mainMenuItems : List Routing.MenuItem
    , currentTime : Time.Time
    , currentTimeDelta : Time.Time
    , user : Entity.User
    , authRecord : Entity.AuthRecord
    , games : List Entity.Game
    , gimages : List Entity.Gimage
    }


type Visitor
    = Anonymous
    | LoggedIn Api.JwtPayload


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
