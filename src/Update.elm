module Update exposing (update)

import Admin.Update as Admin
import Api
import Empty
import Entity
import Http
import Json.Decode
import Json.Decode.Pipeline as JP
import Json.Encode
import Jwt
import Model exposing (..)
import Navigation
import Navigation
import Port
import Process
import Routing exposing (..)
import Task
import Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MessageAdmin msg_ ->
            let
                ( model_, cmds_ ) =
                    Admin.update msg_ model
            in
                ( model_, cmds_ )

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
                            (toFloat jwt.exp) > now

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
                jwtdecoded_ =
                    Api.jwtDecoded auth.token

                ( model_, commands_ ) =
                    case jwtdecoded_ of
                        Ok jwt ->
                            ( { model | spin = False, visitor = LoggedIn jwt, jwtencoded = auth.token }
                            , [ Port.setItem ( "token", auth.token )
                              , Http.send UserResponse (Api.getUser model.api auth.token jwt.sub)
                              ]
                            )

                        Err err ->
                            ( { model | spin = False, error = toString err }, [] )
            in
                ( model_, Cmd.batch commands_ )

        LoginResponse (Err err) ->
            let
                l =
                    Debug.log "login" (toString err)
            in
                ( { model | spin = False, error = "Login related error" }, Cmd.none )

        UserResponse (Ok newUser) ->
            let
                isAdmin =
                    case model.visitor of
                        LoggedIn jwt ->
                            List.map .name jwt.roles
                                |> List.member "admin"

                        _ ->
                            False

                commands =
                    case isAdmin of
                        True ->
                            [ Navigation.newUrl "/admin"
                              --, Http.send UsersResponse Api.getUsers model.api model.jwtencoded
                            ]

                        False ->
                            [ Port.setItem ( "firstName", newUser.firstName )
                            , Navigation.newUrl "/"
                            , Http.send GameResponse (Api.getGame model.api model.jwtencoded "gonogo")
                            , Http.send GameResponse (Api.getGame model.api model.jwtencoded "dotprobe")
                            , Http.send GameResponse (Api.getGame model.api model.jwtencoded "stopsignal")
                            , Http.send GameResponse (Api.getGame model.api model.jwtencoded "respondsignal")
                            , Http.send GameResponse (Api.getGame model.api model.jwtencoded "visualsearch")
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
