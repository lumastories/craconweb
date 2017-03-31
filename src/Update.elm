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
        GroupResp (Ok group) ->
            let
                -- TODO fix this. Why is it not updating the model???
                l =
                    Debug.log "group" <| toString group.slug
            in
                case group.slug of
                    "control_a" ->
                        ( { model | groupIdCon = Just group.id }, Cmd.none )

                    "experimental_a" ->
                        ( { model | groupIdExp = Just group.id }, Cmd.none )

                    _ ->
                        model ! []

        UsersResp (Ok users_) ->
            ( { model
                | users = users_
              }
            , Cmd.none
            )

        SetRegistration key value ->
            let
                userToRegister_old =
                    model.userToRegister

                userToRegister_ =
                    case key of
                        "email" ->
                            { userToRegister_old | email = value }

                        "password" ->
                            { userToRegister_old | password = value }

                        "username" ->
                            { userToRegister_old | username = value }

                        "firstName" ->
                            { userToRegister_old | firstName = value }

                        "lastName" ->
                            { userToRegister_old | lastName = value }

                        _ ->
                            userToRegister_old
            in
                ( { model | userToRegister = userToRegister_ }, Cmd.none )

        TryRegisterUser ->
            ( { model | loading = ( True, "loading..." ) }
            , Cmd.batch
                [ Http.send RegisterUserResp
                    (Api.createUser
                        model.api
                        model.jwtencoded
                        model.userToRegister
                    )
                ]
            )

        RegisterUserResp (Ok newUser) ->
            ( { model | loading = ( False, "" ) }, Navigation.newUrl "/admin" )

        -- SHARED
        ResetNotifications ->
            ( { model
                | glitching = ( False, "" )
                , informing = ( False, "" )
                , loading = ( False, "" )
              }
            , Cmd.none
            ) 
        UpdateLocation path ->
            let
                cmds =
                    [ Navigation.newUrl path
                    , Task.perform VerifyToken Time.now
                    , Port.pinger True
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
                        ( Empty.emptyModel model
                        , [ Port.clearLocalStorage True
                          , Navigation.newUrl "/login"
                          ]
                        )
                    else
                        ( model, [] )
            in
                ( model_, Cmd.batch cmds )

        OnUpdateLocation location ->
            let
                newRoute =
                    parseLocation location
            in
                ( { model
                    | activeRoute = newRoute
                    , isMenuActive = False
                  }
                , Cmd.none
                )

        -- LOGIN
        UpdateEmail newEmail ->
            let
                authRecord_ =
                    model.authRecord
            in
                ( { model | authRecord = { authRecord_ | email = newEmail } }
                , Cmd.none
                )

        UpdatePassword newPassword ->
            let
                authRecord_ =
                    model.authRecord
            in
                ( { model
                    | authRecord = { authRecord_ | password = newPassword }
                  }
                , Cmd.none
                )

        TryLogin ->
            let
                cmd =
                    Http.send LoginResp
                        (Api.postCreds
                            model.api
                            model.authRecord
                        )
            in
                ( { model | loading = ( True, "loading..." ) }, cmd )

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
                            ( { model
                                | loading = ( False, "" )
                                , visitor = LoggedIn jwt
                                , jwtencoded = auth.token
                              }
                            , [ Port.setItem ( "token", auth.token )
                              , Http.send UserResp
                                    (Api.fetchUser
                                        model.api
                                        auth.token
                                        jwt.sub
                                    )
                              ]
                            )

                        Err err ->
                            ( { model
                                | loading = ( False, "" )
                                , glitching = ( True, toString err )
                              }
                            , []
                            )
            in
                ( model_, Cmd.batch commands_ )

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
                ( { model
                    | user = newUser
                    , activeRoute = HomeRoute
                  }
                , Cmd.batch commands
                )

        -- GAMES
        GameResp (Ok game) ->
            ( { model | games = game :: model.games }, Cmd.none )

        GimageResp (Ok gimage) ->
            ( { model | gimages = gimage :: model.gimages }, Cmd.none )

        Presses _ ->
            model ! []

        MainMenuToggle ->
            let
                active =
                    if model.isMenuActive then
                        False
                    else
                        True
            in
                ( { model | isMenuActive = active }, Cmd.none )

        Tick t ->
            ( { model | currentTime = t }, Cmd.none )

        CalcTimeDelta time ->
            ( { model
                | currentTimeDelta = time - model.currentTime
              }
            , Cmd.none
            )

        GetTimeAndThen successHandler ->
            ( model, (Task.perform successHandler Time.now) )

        RoleResp (Ok role) ->
            let
                l =
                    Debug.log "role" role
            in
                ( { model | roleIdUser = Just role.id }, Cmd.none )

        LoginResp (Err err) ->
            (httpErrorState model err)

        UserResp (Err err) ->
            (httpErrorState model err)

        GameResp (Err err) ->
            (httpErrorState model err)

        GimageResp (Err err) ->
            (httpErrorState model err)

        UsersResp (Err err) ->
            (httpErrorState model err)

        RegisterUserResp (Err err) ->
            (httpErrorState model err)

        GroupResp (Err err) ->
            (httpErrorState model err)

        RoleResp (Err err) ->
            (httpErrorState model err)


httpErrorState model err =
    ( { model | loading = ( False, "" ), glitching = ( True, httpHumanError err ), httpErr = toString err }, Cmd.none )


httpHumanError : Http.Error -> String
httpHumanError err =
    case err of
        Http.Timeout ->
            "Something is taking too long"

        Http.NetworkError ->
            "Oops. There's been a network error."

        Http.BadStatus _ ->
            "Server error"

        Http.BadPayload str _ ->
            "Bad payload"

        _ ->
            "Unknown error"


delay : Time.Time -> Msg -> Cmd Msg
delay t msg =
    Process.sleep t |> Task.perform (\_ -> msg)
