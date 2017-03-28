module Update exposing (update)

import Model exposing (..)
import Http
import Navigation
import Port
import Json.Encode
import Json.Decode
import Json.Decode.Pipeline as JP
import Navigation
import Routing exposing (..)
import Jwt
import Time
import Task
import Entity
import Process
import Empty
import Api


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Routing
        UpdateLocation path ->
            let
                cmds =
                    [ Navigation.newUrl path
                    , Task.perform VerifyToken Time.now
                    ]
            in
                ( model, Cmd.batch cmds )

        VerifyToken now ->
            let
                expired =
                    case model.visitor of
                        LoggedIn jwt ->
                            (toFloat jwt.exp) < now

                        _ ->
                            True

                ( model_, cmds ) =
                    if expired then
                        ( Empty.emptyModel model, [ Port.clearLocalStorage True, Navigation.newUrl "/login" ] )
                    else
                        ( model, [] )
            in
                ( model_, Cmd.batch cmds )

        OnUpdateLocation location ->
            let
                newRoute =
                    parseLocation location
            in
                ( { model | activeRoute = newRoute, menuIsActive = False }, Cmd.none )

        -- Actions
        UpdateEmail newEmail ->
            let
                authRecord_ =
                    model.authRecord
            in
                ( { model | authRecord = { authRecord_ | email = newEmail } }, Cmd.none )

        UpdatePassword newPassword ->
            let
                authRecord_ =
                    model.authRecord
            in
                ( { model | authRecord = { authRecord_ | password = newPassword } }, Cmd.none )

        TryLogin ->
            let
                cmd =
                    Http.send LoginResponse (Api.postCreds model.api model.authRecord)
            in
                ( { model | spin = True }, cmd )

        -- HTTP Responses
        LoginResponse (Ok auth) ->
            let
                commands =
                    case model.visitor of
                        LoggedIn jwt ->
                            [ Port.setItem ( "token", auth.token )
                            , Http.send UserResponse (Api.getUser model.api auth.token jwt.sub)
                            ]

                        Anonymous ->
                            []
            in
                ( { model | spin = False }, Cmd.batch commands )

        LoginResponse (Err err) ->
            let
                l =
                    Debug.log "login" (toString err)
            in
                ( { model | spin = False, error = "Login related error" }, Cmd.none )

        UserResponse (Ok newUser) ->
            let
                getGame_ s =
                    Api.getGame model.api model.jwtencoded s

                commands =
                    [ Port.setItem ( "firstName", newUser.firstName )
                    , Navigation.newUrl "/"
                    , Http.send GameResponse (getGame_ "gonogo")
                    , Http.send GameResponse (getGame_ "dotprobe")
                    , Http.send GameResponse (getGame_ "stopsignal")
                    , Http.send GameResponse (getGame_ "respondsignal")
                    , Http.send GameResponse (getGame_ "visualsearch")
                    ]
            in
                ( { model | user = newUser, activeRoute = HomeRoute }, Cmd.batch commands )

        UserResponse (Err err) ->
            let
                l =
                    Debug.log "login" (toString err)
            in
                ( { model | error = "User related error" }, Cmd.none )

        GameResponse (Ok game) ->
            ( { model | games = game :: model.games }, Cmd.none )

        GameResponse (Err err) ->
            ( { model | error = (toString err) }, Cmd.none )

        GimageResponse (Ok gimage) ->
            ( { model | gimages = gimage :: model.gimages }, Cmd.none )

        GimageResponse (Err err) ->
            model ! []

        Presses _ ->
            model ! []

        MainMenuToggle ->
            let
                active =
                    if model.menuIsActive then
                        False
                    else
                        True
            in
                ( { model | menuIsActive = active }, Cmd.none )

        Logout ->
            let
                cmds =
                    [ Port.clearLocalStorage True
                    , Navigation.newUrl "/login"
                    ]
            in
                ( Empty.emptyModel model, Cmd.batch cmds )

        Tick t ->
            ( { model | currentTime = t }, Cmd.none )

        CalcTimeDelta time ->
            ( { model | currentTimeDelta = time - model.currentTime }, Cmd.none )

        GetTimeAndThen successHandler ->
            ( model, (Task.perform successHandler Time.now) )


delay : Time.Time -> Msg -> Cmd Msg
delay t msg =
    Process.sleep t |> Task.perform (\_ -> msg)



-- delay (Time.Time.millisecond*500) cmdMsg
