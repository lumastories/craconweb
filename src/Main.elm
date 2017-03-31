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
        ( api_, jwtdecoded_, jwtencoded_ ) =
            ( "http://localhost:8680"
            , Api.jwtDecoded flags.token
            , flags.token
            )

        ( visitor_, route_, commands_ ) =
            case jwtdecoded_ of
                Ok jwtdecoded ->
                    ( Model.LoggedIn jwtdecoded
                    , Routing.parseLocation location
                    , initData api_ jwtencoded_
                    )

                Err _ ->
                    ( Model.Anonymous, Routing.LoginRoute, [] )

        initModel =
            { api = api_
            , jwtencoded = jwtencoded_
            , activeRoute = route_
            , presses = []
            , visitor = visitor_
            , isMenuActive = False
            , mainMenuItems = Routing.initMenuItems
            , currentTime = 0
            , currentTimeDelta = 0
            , user = Empty.emptyUser
            , authRecord = Empty.emptyAuthRecord
            , games = []
            , gimages = []
            , loading = ( False, "" )
            , glitching = ( False, "" )
            , informing = ( False, "" )
            , users = []
            , userToRegister = Empty.emptyUserRecord
            , roleIdUser = Nothing
            , groupIdExp = Nothing
            , groupIdCon = Nothing
            }
    in
        ( initModel, Cmd.batch commands_ )


type alias Flags =
    { token : String
    }



-- TODO Fix this - When should we fetch data? Account for refresh and user.


initData : String -> String -> List (Cmd Model.Msg)
initData api token =
    -- Game stuff
    [ Http.send Model.GameResp (Api.getGame api token "gonogo")
    , Http.send Model.GameResp (Api.getGame api token "dotprobe")
    , Http.send Model.GameResp (Api.getGame api token "stopsignal")
    , Http.send Model.GameResp (Api.getGame api token "respondsignal")
    , Http.send Model.GameResp (Api.getGame api token "visualsearch")
      -- Admin stuff
    , Http.send Model.UsersResp (Api.fetchUsers api token)
    , Http.send Model.GroupResp (Api.getGroup api token "control_a")
    , Http.send Model.GroupResp (Api.getGroup api token "experimental_a")
    , Http.send Model.RoleResp (Api.getRole api token "user")
    ]
