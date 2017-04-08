module Main exposing (..)

--import Api

import Char
import Empty
import Keyboard exposing (..)
import Model as M
import Navigation
import Routing as R


--import Time

import Update
import View
import Port


--import Task


main : Program Flags M.Model M.Msg
main =
    Navigation.programWithFlags M.OnUpdateLocation
        { init = init
        , view = View.view
        , update = Update.update
        , subscriptions = subscriptions
        }


subscriptions : M.Model -> Sub M.Msg
subscriptions model =
    Sub.batch
        [ Keyboard.presses (\code -> M.Presses (Char.fromCode code))
          --, Time.every Time.second M.NewCurrentTime
        , Port.user M.SetUser
        , Port.status M.SetStatus
        ]


init : Flags -> Navigation.Location -> ( M.Model, Cmd M.Msg )
init flags location =
    let
        ( httpsrv, tasksrv, filesrv ) =
            servers location.hostname

        ( route_, visitor_, commands_ ) =
            genesis flags

        baseModel =
            { httpsrv = httpsrv
            , tasksrv = tasksrv
            , filesrv = filesrv
            , jwtencoded = flags.token
            , activeRoute = route_
            , presses = []
            , visitor = visitor_
            , isMenuActive = False
            , mainMenuItems = R.initMenuItems
            , currentTime = 0
            , currentTimeDelta = 0
            , user = Empty.emptyUser
            , authRecord = Empty.emptyAuthRecord
            , validImages = []
            , invalidImages = []
            , fillerImages = []
            , userGroupId = Nothing
            , loading = Nothing
            , glitching = Nothing
            , informing = Nothing
            , users = []
            , tmpUserRecord = Empty.emptyUserRecord
            , userRole = Empty.emptyRole
            , groupIdExp = Nothing
            , groupIdCon = Nothing
            , httpErr = ""
            , gonogoGame = Empty.emptyGame
            , dotprobeGame = Empty.emptyGame
            , stopsignalGame = Empty.emptyGame
            , respondsignalGame = Empty.emptyGame
            , visualsearchGame = Empty.emptyGame
            , responseTimes = []
            , startTime = 0
            , playingGame = False
            , myGroupSlug = Nothing
            , csvId = "CsvInputId"
            , mCsvFile = Nothing
            , theUserId = Nothing
            }
    in
        ( baseModel, Cmd.batch commands_ )


type alias Flags =
    { token : String
    , time : Float
    }


servers : String -> ( String, String, String )
servers hostname =
    case hostname of
        "localhost" ->
            ( "http://localhost:8680"
            , "http://localhost:8668"
            , "http://localhost:8654"
            )

        _ ->
            ( "https://httpsrv.cravecontrol.org"
            , "https://tasksrv.cravecontrol.org"
            , "https://filesrv.cravecontrol.org"
            )


genesis : Flags -> ( R.Route, M.Visitor, List (Cmd M.Msg) )
genesis flags =
    ( R.LoginRoute, M.Anonymous, [] )
