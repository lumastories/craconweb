module Main exposing (..)

import Api
import Char
import Empty
import Http
import Keyboard exposing (..)
import Model
import Navigation
import Routing
import Time
import Update
import View


main : Program Flags Model.Model Model.Msg
main =
    Navigation.programWithFlags Model.OnUpdateLocation
        { init = init
        , view = View.view
        , update = Update.update
        , subscriptions = subscriptions
        }


subscriptions : Model.Model -> Sub Model.Msg
subscriptions model =
    Sub.batch
        [ Keyboard.presses (\code -> Model.Presses (Char.fromCode code))
          -- , Time.every Time.millisecond Model.Tick
        ]


init : Flags -> Navigation.Location -> ( Model.Model, Cmd Model.Msg )
init flags location =
    let
        -- if token exists check if token is expired then Routing.LoginRoute
        -- else check if is not admin block /admin and /register routes
        -- else Routing.parseLocation location
        -- else (not exist - login route, anon user)
        api_ =
            "http://localhost:8680"

        blockAdminRoutes : Navigation.Location -> Routing.Route
        blockAdminRoutes location =
            case location.pathname of
                "/admin" ->
                    Routing.HomeRoute

                "/register" ->
                    Routing.HomeRoute

                _ ->
                    (Routing.parseLocation location)

        ( route_, visitor_ ) =
            case flags.token of
                "" ->
                    ( Routing.LoginRoute, Model.Anonymous )

                _ ->
                    case Api.isOld flags.time flags.token of
                        True ->
                            ( Routing.LoginRoute, Model.Anonymous )

                        False ->
                            case Api.jwtDecoded flags.token of
                                Ok jwt ->
                                    case (List.map .name jwt.roles |> List.member "admin") of
                                        True ->
                                            ( Routing.parseLocation location, Model.LoggedIn jwt )

                                        False ->
                                            ( blockAdminRoutes location, Model.LoggedIn jwt )

                                _ ->
                                    ( Routing.LoginRoute, Model.Anonymous )

        commands_ =
            initData api_ flags.token

        initModel =
            { api = api_
            , jwtencoded = flags.token
            , activeRoute = route_
            , presses = []
            , visitor = visitor_
            , isMenuActive = False
            , mainMenuItems = Routing.initMenuItems
            , currentTime = 0
            , currentTimeDelta = 0
            , user = Empty.emptyUser
            , authRecord = Empty.emptyAuthRecord
            , gimages = []
            , loading = ( False, "" )
            , glitching = ( False, "" )
            , informing = ( False, "" )
            , users = []
            , tmpUserRecord = Empty.emptyUserRecord
            , userRole = Empty.emptyUserRole
            , groupIdExp = Nothing
            , groupIdCon = Nothing
            , httpErr = ""
            , gonogoGame = Empty.emptyGame
            , dotprobeGame = Empty.emptyGame
            , stopsignalGame = Empty.emptyGame
            , respondsignalGame = Empty.emptyGame
            , visualsearchGame = Empty.emptyGame
            }
    in
        ( initModel, Cmd.batch commands_ )


type alias Flags =
    { token : String
    , time : Float
    }



-- TODO Fix this - When should we fetch data? Account for refresh and user.


initData : String -> String -> List (Cmd Model.Msg)
initData api token =
    -- Game stuff
    [ Http.send Model.GameResp (Api.fetchGame api token "gonogo")
    , Http.send Model.GameResp (Api.fetchGame api token "dotprobe")
    , Http.send Model.GameResp (Api.fetchGame api token "stopsignal")
    , Http.send Model.GameResp (Api.fetchGame api token "respondsignal")
    , Http.send Model.GameResp (Api.fetchGame api token "visualsearch")
      -- Admin stuff
    , Http.send Model.UsersResp (Api.fetchUsers api token)
    , Http.send Model.GroupResp (Api.fetchGroup api token "control_a")
    , Http.send Model.GroupResp (Api.fetchGroup api token "experimental_a")
    , Http.send Model.RoleResp (Api.fetchRole api token "user")
    ]
