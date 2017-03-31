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
    , activeRoute : Routing.Route
    , presses : List Char
    , visitor : Visitor
    , isMenuActive : Bool
    , mainMenuItems : List Routing.MenuItem
    , currentTime : Time.Time
    , currentTimeDelta : Time.Time
    , user : Entity.User
    , authRecord : Entity.AuthRecord
    , games : List Entity.Game
    , gimages : List Entity.Gimage
    , loading : ( Bool, String )
    , glitching : ( Bool, String )
    , informing : ( Bool, String )
    , users : List Entity.User
    , userToRegister : Entity.UserRecord
    , roleIdUser : Maybe String
    , groupIdExp : Maybe String
    , groupIdCon : Maybe String
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
    | ResetNotifications
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
    | UsersResp (Result Http.Error (List Entity.User))
    | RegisterUserResp (Result Http.Error Entity.UserRecord)
    | GroupResp (Result Http.Error Entity.Group)
    | RoleResp (Result Http.Error Entity.Role)
      -- ADMIN
    | TryRegisterUser
    | SetRegistration String String
