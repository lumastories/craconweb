module Update exposing (update)

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
        -- ADMIN
        ConGroupResp (Ok group) ->
            ( { model | adminModel = (setConGroupId model group.id) }, Cmd.none )

        ConGroupResp (Err err) ->
            model ! []

        ExpGroupResp (Ok group) ->
            ( { model | adminModel = (setExpGroupId model group.id) }, Cmd.none )

        ExpGroupResp (Err err) ->
            model ! []

        UsersResp (Ok usersList) ->
            ( { model | error = "no admin yet", adminModel = (setUsers model usersList.users) }, Cmd.none )

        UsersResp (Err err) ->
            ( { model | error = toString err }, Cmd.none )

        SetRegistration key value ->
            let
                ( userToRegister_, adminModel_ ) =
                    ( model.adminModel.userToRegister, model.adminModel )

                newUserToRegister =
                    case key of
                        "email" ->
                            { userToRegister_ | email = value }

                        "password" ->
                            -- TODO handle password
                            userToRegister_

                        "username" ->
                            { userToRegister_ | username = value }

                        "firstName" ->
                            { userToRegister_ | firstName = value }

                        "lastName" ->
                            { userToRegister_ | lastName = value }

                        _ ->
                            userToRegister_

                newAdminModel =
                    { adminModel_ | userToRegister = newUserToRegister }
            in
                ( { model | adminModel = newAdminModel }, Cmd.none )

        TryRegisterUser ->
            model ! []

        RegisterUserResp (Ok newUser) ->
            model ! []

        RegisterUserResp (Err err) ->
            model ! []

        -- SHARED
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

        -- LOGIN
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
                    Http.send LoginResp (Api.postCreds model.api model.authRecord)
            in
                ( { model | spin = True }, cmd )

        Logout ->
            let
                cmds =
                    [ Port.clearLocalStorage True
                    , Navigation.newUrl "/login"
                    ]
            in
                ( Empty.emptyModel model, Cmd.batch cmds )

        LoginResp (Ok auth) ->
            let
                jwtdecoded_ =
                    Api.jwtDecoded auth.token

                ( model_, commands_ ) =
                    case jwtdecoded_ of
                        Ok jwt ->
                            ( { model | spin = False, visitor = LoggedIn jwt, jwtencoded = auth.token }
                            , [ Port.setItem ( "token", auth.token )
                              , Http.send UserResp (Api.getUser model.api auth.token jwt.sub)
                              ]
                            )

                        Err err ->
                            ( { model | spin = False, error = toString err }, [] )
            in
                ( model_, Cmd.batch commands_ )

        LoginResp (Err err) ->
            let
                l =
                    Debug.log "login" (toString err)
            in
                ( { model | spin = False, error = "Login related error" }, Cmd.none )

        UserResp (Ok newUser) ->
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
                            ]

                        False ->
                            [ Port.setItem ( "firstName", newUser.firstName )
                            , Navigation.newUrl "/"
                            ]
            in
                ( { model | user = newUser, activeRoute = HomeRoute }, Cmd.batch commands )

        UserResp (Err err) ->
            let
                l =
                    Debug.log "login" (toString err)
            in
                ( { model | error = "User related error" }, Cmd.none )

        -- GAMES
        GameResp (Ok game) ->
            ( { model | games = game :: model.games }, Cmd.none )

        GameResp (Err err) ->
            ( { model | error = (toString err) }, Cmd.none )

        GimageResp (Ok gimage) ->
            ( { model | gimages = gimage :: model.gimages }, Cmd.none )

        GimageResp (Err err) ->
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

        Tick t ->
            ( { model | currentTime = t }, Cmd.none )

        CalcTimeDelta time ->
            ( { model | currentTimeDelta = time - model.currentTime }, Cmd.none )

        GetTimeAndThen successHandler ->
            ( model, (Task.perform successHandler Time.now) )


delay : Time.Time -> Msg -> Cmd Msg
delay t msg =
    Process.sleep t |> Task.perform (\_ -> msg)


setUsers : Model -> List Entity.User -> AdminModel
setUsers model users_ =
    let
        adminModel =
            model.adminModel
    in
        { adminModel | users = users_ }


setConGroupId : Model -> Int -> AdminModel
setConGroupId model conGroupId_ =
    let
        adminModel =
            model.adminModel
    in
        { adminModel | conGroupId = conGroupId_ }


setExpGroupId : Model -> Int -> AdminModel
setExpGroupId model expGroupId_ =
    let
        adminModel =
            model.adminModel
    in
        { adminModel | conGroupId = expGroupId_ }



-- delay (Time.Time.millisecond*500) cmdMsg
