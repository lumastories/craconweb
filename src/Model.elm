module Model exposing (..)

import Api
import Entity
import Http
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
    , validImages : List Entity.Ugimage
    , invalidImages : List Entity.Ugimage
    , fillerImages : List Entity.Ugimage
    , userGroupId : Maybe String
    , loading : Maybe String
    , glitching : Maybe String
    , informing : Maybe String
    , users : List Entity.User
    , tmpUserRecord : Entity.UserRecord
    , userRole : Entity.Role
    , groupIdExp : Maybe String
    , groupIdCon : Maybe String
    , httpErr : String
    , gonogoGame : Entity.Game
    , dotprobeGame : Entity.Game
    , stopsignalGame : Entity.Game
    , respondsignalGame : Entity.Game
    , visualsearchGame : Entity.Game
    , responseTimes : List Time.Time
    , startTime : Time.Time
    , playingGame : Bool
    , myGroupSlug : Maybe String
    , csvId : String
    , mCsvFile : Maybe Api.CsvData
    , theUserId : Maybe String
    }


type Visitor
    = Anonymous
    | LoggedIn Api.JwtPayload


type alias Slug =
    String


type
    Msg
    -- SHARED
    = UpdateLocation String
    | OnUpdateLocation Navigation.Location
    | GetStoredUser String
    | UpdateEmail String
    | UpdatePassword String
    | TryLogin
    | Logout
    | ResetNotifications
    | MainMenuToggle
      -- GAMES
    | NewCurrentTime Time.Time
    | Presses Char
    | StartGameWith Time.Time
    | Tick Time.Time
    | PlayGame Slug
    | StopPlaying Slug
      -- HTTP
    | LoginResp (Result Http.Error Entity.Auth)
    | UserResp (Result Http.Error Entity.User)
    | GameResp (Result Http.Error Entity.Game)
    | UsersResp (Result Http.Error (List Entity.User))
    | RegisterUserResp (Result Http.Error Entity.User)
    | GroupResp (Result Http.Error Entity.Group)
    | RoleResp (Result Http.Error Entity.Role)
    | CsvUploadResp (Result Http.Error Api.CsvData)
      -- ADMIN
    | TryRegisterUser
    | SetRegistration String String
    | EditUserAccount String String
    | CsvSelected
    | CsvRead Api.CsvData
    | TryCsvUpload
