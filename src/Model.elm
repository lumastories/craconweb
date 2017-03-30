module Model exposing (..)

import Api
import Entity
import Http
import Jwt
import Navigation
import Routing
import Time


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
    , adminModel : AdminModel
    , errNotif : Bool
    }


type alias AdminModel =
    { users : List Entity.User
    , userToRegister : Entity.UserRecord
    , conGroupId : Int
    , expGroupId : Int
    }


type Visitor
    = Anonymous
    | LoggedIn Api.JwtPayload


type
    Msg
    -- SHARED
    = UpdateLocation String
    | OnUpdateLocation Navigation.Location
    | UpdateEmail String
    | UpdatePassword String
    | TryLogin
    | Logout
    | HideErrorNotification
      -- GAMES
    | Presses Char
    | MainMenuToggle
    | GetTimeAndThen (Time.Time -> Msg)
    | CalcTimeDelta Time.Time
    | Tick Time.Time
    | VerifyToken Time.Time
      -- HTTP
    | LoginResp (Result Http.Error Entity.Auth)
    | UserResp (Result Http.Error Entity.User)
    | GameResp (Result Http.Error Entity.Game)
    | GimageResp (Result Http.Error Entity.Gimage)
    | UsersResp (Result Http.Error Api.UsersList)
    | RegisterUserResp (Result Http.Error Entity.User)
    | ConGroupResp (Result Http.Error Entity.Group)
    | ExpGroupResp (Result Http.Error Entity.Group)
      -- ADMIN
    | TryRegisterUser
    | SetRegistration String String
