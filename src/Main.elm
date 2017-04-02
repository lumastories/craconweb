module Main exposing (..)

import Api
import Char
import Empty
import Keyboard exposing (..)
import Model
import Navigation
import Routing
import Time
import Update
import View
import Todos
import Port
import Task


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
          -- , Time.every Time.minute Model.VerifyToken
          --, Port.storageGetItemResponse Model.ReceiveFromLocalStorage
        , Port.getUserResponse Model.GetStoredUser
        ]


init : Flags -> Navigation.Location -> ( Model.Model, Cmd Model.Msg )
init flags location =
    let
        -- if token exists check if token is expired then Routing.LoginRoute
        -- else check if is not admin block /admin and /register routes
        -- else Routing.parseLocation location
        -- else (not exist - login route, anon user)
        api_ =
            case location.hostname of
                "localhost" ->
                    "http://localhost:8680"

                _ ->
                    "https://api.cravecontrol.org"

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
            case visitor_ of
                Model.Anonymous ->
                    [ Task.perform Model.NewCurrentTime Time.now ]

                Model.LoggedIn _ ->
                    (Todos.initCommands api_ flags.token) ++ [ Task.perform Model.NewCurrentTime Time.now ]

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
            , validImages = []
            , invalidImages = []
            , fillerImages = []
            , userGroupId = Nothing
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
            , responseTimes = []
            , startTime = 0
            , playingGame = False
            , myGroupSlug = Nothing
            }
    in
        ( initModel, Cmd.batch commands_ )


type alias Flags =
    { token : String
    , time : Float
    }
