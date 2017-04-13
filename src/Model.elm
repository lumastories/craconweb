module Model exposing (..)

import Entity
import Http
import GameManager
import Navigation
import Routing
import Time
import Json.Decode as JD
import Json.Encode as JE
import Json.Decode.Pipeline as JP


type alias Model =
    { httpsrv : String
    , tasksrv : String
    , filesrv : String
    , jwtencoded : String
    , activeRoute : Routing.Route
    , presses : List Char
    , visitor : Visitor
    , isMenuActive : Bool
    , mainMenuItems : List Routing.MenuItem
    , currentTime : Time.Time
    , currentTimeDelta : Time.Time
    , user : Maybe Entity.User
    , authRecord : Entity.AuthRecord
    , validImages : Maybe (List Entity.Ugimage)
    , invalidImages : Maybe (List Entity.Ugimage)
    , fillerImages : Maybe (List Entity.Ugimage)
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
    , gonogoGame : Maybe Entity.Game
    , dotprobeGame : Maybe Entity.Game
    , stopsignalGame : Maybe Entity.Game
    , respondsignalGame : Maybe Entity.Game
    , visualsearchGame : Maybe Entity.Game
    , responseTimes : List Time.Time
    , startTime : Time.Time
    , playingGame : Maybe (GameManager.Game Msg)
    , ugimgsets : Maybe (List Entity.Ugimgset)
    }


type ValuationsError
    = ReqFail Http.Error
    | MissingValuations


type Visitor
    = Anon
    | LoggedIn JwtPayload


type alias JwtPayload =
    { aud : String
    , exp : Float
    , iat : Int
    , iss : String
    , sub : String
    , roles : List Entity.Role
    }


type Msg
    = UpdateLocation String
    | OnUpdateLocation Navigation.Location
    | SetStatus String
    | UpdateEmail String
    | UpdatePassword String
    | TryLogin
    | Logout
    | ResetNotifications
    | MainMenuToggle
    | NewCurrentTime Time.Time
    | Presses Int
    | StartGameWith Time.Time
    | Tick Time.Time
    | IntIndication Int
    | InitStopSignal
    | InitGoNoGo
    | InitDotProbe
    | InitRespondSignal
    | InitVisualSearch
    | PlayGame (GameManager.Game Msg)
    | StopGame
    | AuthResp (Result Http.Error Entity.Auth)
    | UserResp (Result Http.Error Entity.User)
    | GameResp (Result Http.Error Entity.Game)
    | UsersResp (Result Http.Error (List Entity.User))
    | RegisterUserResp (Result Http.Error Entity.User)
    | GroupResp (Result Http.Error Entity.Group)
    | RoleResp (Result Http.Error Entity.Role)
    | FillerResp (Result ValuationsError (List Entity.Ugimage))
    | ValidResp (Result ValuationsError (List Entity.Ugimage))
    | InvalidResp (Result ValuationsError (List Entity.Ugimage))
    | TryRegisterUser
    | SetRegistration String String
    | TryUpdateUser
    | EditUserAccount String String


ugimgsetsDecoder : JD.Decoder (List Entity.Ugimgset)
ugimgsetsDecoder =
    JD.field "ugimgsets" (JD.list Entity.ugimgsetDecoder)
        |> JD.maybe
        |> JD.map (Maybe.withDefault [])


ugimageDecoder : JD.Decoder (List Entity.Ugimage)
ugimageDecoder =
    JD.field "ugimages" (JD.list Entity.ugimageDecoder)


type alias ErrorCode =
    { error : String
    , code : Int
    }


errorCodeEncoder : String -> ErrorCode
errorCodeEncoder errorCode =
    case JD.decodeString errorCodeDecoder errorCode of
        Ok ed ->
            ed

        Err _ ->
            { error = "error"
            , code = 0
            }


tokenEncoder : String -> JE.Value
tokenEncoder token =
    JE.object [ ( "token", JE.string token ) ]


errorCodeDecoder : JD.Decoder ErrorCode
errorCodeDecoder =
    JP.decode ErrorCode
        |> JP.required "error" JD.string
        |> JP.required "code" JD.int


jwtDecoder : JD.Decoder JwtPayload
jwtDecoder =
    JP.decode JwtPayload
        |> JP.required "aud" (JD.string)
        |> JP.required "exp" (JD.float)
        |> JP.required "iat" (JD.int)
        |> JP.required "iss" (JD.string)
        |> JP.required "sub" (JD.string)
        |> JP.required "roles" (JD.list Entity.roleDecoder)
