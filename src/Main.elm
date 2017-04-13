module Main exposing (..)

import Api
import Empty
import Keyboard
import Model as M
import Navigation
import Routing as R
import Update
import View
import Port
import Time


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
        [ Keyboard.presses M.Presses
        , ticker model.playingGame
        , Port.status M.SetStatus
        ]


ticker : Maybe a -> Sub M.Msg
ticker playingGame =
    playingGame
        |> Maybe.map (\_ -> Time.every Time.millisecond M.NewCurrentTime)
        |> Maybe.withDefault Sub.none


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
            , user = Nothing
            , authRecord = Empty.emptyAuthRecord
            , ugimages_v = Nothing
            , ugimages_i = Nothing
            , ugimages_f = Nothing
            , loading = Nothing
            , glitching = Nothing
            , informing = Nothing
            , users = []
            , tmpUserRecord = Empty.emptyUserRecord
            , userRole = Empty.emptyRole
            , groupIdExp = Nothing
            , groupIdCon = Nothing
            , httpErr = ""
            , gonogoGame = Nothing
            , dotprobeGame = Nothing
            , stopsignalGame = Nothing
            , respondsignalGame = Nothing
            , visualsearchGame = Nothing
            , playingGame = Nothing
            , ugimgsets = Nothing
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
            ( "https://api.cravecontrol.org"
            , "https://task.cravecontrol.org"
            , "https://file.cravecontrol.org"
            )
