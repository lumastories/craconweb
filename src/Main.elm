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
        api_ =
            "http://localhost:8680"

        jwtdecoded_ =
            Api.jwtDecoded flags.token

        activeRoute_ =
            case jwtdecoded_ of
                Ok _ ->
                    Routing.parseLocation location

                Err _ ->
                    Routing.LoginRoute

        initCommands =
            case jwtdecoded_ of
                Ok _ ->
                    Api.initData api_ flags.token

                Err _ ->
                    []

        initModel =
            { authRecord = Empty.emptyAuthRecord
            , spin = False
            , activeRoute = activeRoute_
            , changes = 0
            , api = api_
            , jwtencoded = flags.token
            , jwtdecoded = jwtdecoded_
            , error = ""
            , presses = []
            , user = Empty.emptyUser
            , menuIsActive = False
            , mainMenuItems = Routing.initMenuItems
            , greeting = ""
            , test = ""
            , currentTime = 0
            , currentTimeDelta = 0
            , games = []
            , gimages = []
            }
    in
        ( initModel, Cmd.batch initCommands )


type alias Flags =
    { token : String
    , firstName : String
    }
