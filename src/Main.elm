module Main exposing (..)

import Api
import Char
import Empty
import Keyboard exposing (..)
import Model as M
import Navigation
import Routing as R
import Update
import View
import Port


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

        -- based on location and jwt
        ( route_, visitor_, commands_ ) =
            case Api.okyToky flags.time flags.token of
                Ok jwt ->
                    ( R.parseLocation location
                    , M.LoggedIn jwt
                    , Api.fetchAll httpsrv jwt flags.token
                    )

                Err _ ->
                    ( R.LoginRoute
                    , M.Anon
                    , Navigation.newUrl R.loginPath
                    )

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
            }
    in
        ( baseModel, commands_ )


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
