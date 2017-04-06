module Update exposing (update)

import Api
import Empty
import Entity
import Http
import Json.Decode as JD
import Json.Encode as JE
import Model exposing (..)
import Navigation
import Navigation
import Port
import Process
import Routing exposing (..)
import Task
import Time
import Todos


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TryUpdateUser userId ->
            ( model, Port.uploadFile userId )

        CsvUploadResp csvData ->
            -- TODO make a request to set local ugimages!
            -- And to fetch them from filesrv
            ( model, Cmd.none )

        CsvSelected ->
            ( model, Port.fileSelected model.csvId )

        CsvRead data ->
            let
                newCsvFile =
                    Just
                        { upload = data.upload
                        , userid = data.userid
                        }
            in
                ( { model | mCsvFile = newCsvFile }, Cmd.none )

        NewCurrentTime now ->
            {- if playing game, calculate time since game started
               display stims according to Entity.Game rules
               (if now - lastOnset >= 1250, goto next trial)
                   validImages
                   invalidImages
                   fillerImages
            -}
            ( { model | currentTime = now }, Cmd.none )

        -- ADMIN
        GroupResp (Ok group) ->
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
                tmpUserRecord_old_ =
                    model.tmpUserRecord

                tmpUserRecord_old =
                    { tmpUserRecord_old_ | roles = [ model.userRole.id ] }

                tmpUserRecord_ =
                    case key of
                        "email" ->
                            { tmpUserRecord_old | email = value }

                        "password" ->
                            { tmpUserRecord_old | password = value }

                        "username" ->
                            { tmpUserRecord_old | username = value }

                        "firstName" ->
                            { tmpUserRecord_old | firstName = value }

                        "lastName" ->
                            { tmpUserRecord_old | lastName = value }

                        _ ->
                            tmpUserRecord_old
            in
                ( { model | tmpUserRecord = tmpUserRecord_ }, Cmd.none )

        EditUserAccount key value ->
            let
                tmpUserRecord_old_ =
                    model.tmpUserRecord

                tmpUserRecord_old =
                    { tmpUserRecord_old_ | roles = [ model.userRole.id ] }

                tmpUserRecord_ =
                    case key of
                        "email" ->
                            { tmpUserRecord_old | email = value }

                        "password" ->
                            { tmpUserRecord_old | password = value }

                        "username" ->
                            { tmpUserRecord_old | username = value }

                        "firstName" ->
                            { tmpUserRecord_old | firstName = value }

                        "lastName" ->
                            { tmpUserRecord_old | lastName = value }

                        _ ->
                            tmpUserRecord_old
            in
                ( { model | tmpUserRecord = tmpUserRecord_ }, Cmd.none )

        TryRegisterUser ->
            ( { model | loading = Just "loading..." }
            , Cmd.batch
                [ Http.send RegisterUserResp
                    (Api.createUserRecord
                        model.api
                        model.jwtencoded
                        model.tmpUserRecord
                    )
                ]
            )

        RegisterUserResp (Ok newUser) ->
            let
                users_ =
                    [ newUser ] ++ model.users
            in
                ( { model | loading = Nothing, users = users_, tmpUserRecord = Empty.emptyUserRecord }, Navigation.newUrl Routing.adminPath )

        -- SHARED
        ResetNotifications ->
            ( { model
                | glitching = Nothing
                , informing = Nothing
                , loading = Nothing
              }
            , Cmd.none
            )

        UpdateLocation path ->
            let
                cmds =
                    [ Navigation.newUrl path
                      --, Port.pinger True
                    ]
            in
                ( model, Cmd.batch cmds )

        OnUpdateLocation location ->
            ( { model
                | activeRoute = parseLocation location
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
                        (Api.createAuthRecord
                            model.api
                            model.authRecord
                        )
            in
                ( { model | loading = Just "loading..." }, cmd )

        Logout ->
            let
                cmds =
                    [ Port.storageClear ()
                    , Navigation.newUrl "/login"
                    , Port.playAudioPing ()
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
                                | loading = Nothing
                                , visitor = LoggedIn jwt
                                , jwtencoded = auth.token
                                , glitching = Nothing
                              }
                            , [ Port.storageSetItem ( "token", JE.object [ ( "token", JE.string auth.token ) ] )
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
                                | loading = Nothing
                                , glitching = Just (toString err)
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
                            [ Port.storageSetItem ( "user", Entity.userEncoder newUser )
                            , Navigation.newUrl Routing.adminPath
                            ]
                                ++ (Todos.initCommands model.api model.jwtencoded)

                        False ->
                            [ Port.storageSetItem ( "user", Entity.userEncoder newUser )
                            , Navigation.newUrl "/"
                            ]
                                ++ (Todos.initCommands model.api model.jwtencoded)
            in
                ( { model
                    | user = newUser
                    , activeRoute = HomeRoute
                  }
                , Cmd.batch commands
                )

        -- GAMES
        PlayGame slug ->
            ( { model | playingGame = True }, Cmd.none )

        StopPlaying slug ->
            ( { model | playingGame = False }, Cmd.none )

        GameResp (Ok game) ->
            case game.slug of
                "gonogo" ->
                    ( { model | gonogoGame = game }, Cmd.none )

                "dotprobe" ->
                    ( { model | dotprobeGame = game }, Cmd.none )

                "stopsignal" ->
                    ( { model | stopsignalGame = game }, Cmd.none )

                "respondsignal" ->
                    ( { model | respondsignalGame = game }, Cmd.none )

                "visualsearch" ->
                    ( { model | visualsearchGame = game }, Cmd.none )

                _ ->
                    model ! []

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

        StartGameWith time ->
            ( { model
                | startTime = time
              }
            , Cmd.none
            )

        RoleResp (Ok role) ->
            ( { model | userRole = role }, Cmd.none )

        LoginResp (Err err) ->
            (httpErrorState model err)

        UserResp (Err err) ->
            (httpErrorState model err)

        GameResp (Err err) ->
            (httpErrorState model err)

        UsersResp (Err err) ->
            (httpErrorState model err)

        RegisterUserResp (Err err) ->
            (httpErrorState model err)

        GroupResp (Err err) ->
            (httpErrorState model err)

        RoleResp (Err err) ->
            (httpErrorState model err)

        GetStoredUser string ->
            case JD.decodeString Entity.userDecoder string of
                Ok user_ ->
                    ( { model | user = user_ }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


httpErrorState : Model -> Http.Error -> ( Model, Cmd msg )
httpErrorState model err =
    ( { model
        | loading = Nothing
        , glitching = Just (httpHumanError err)
        , httpErr = toString err
      }
    , Cmd.none
    )


httpHumanError : Http.Error -> String
httpHumanError err =
    case err of
        Http.Timeout ->
            "Something is taking too long"

        Http.NetworkError ->
            "Oops. There's been a network error."

        Http.BadStatus s ->
            "Server error: " ++ (.error (Api.decodeErrorCode s.body))

        Http.BadPayload str _ ->
            "Bad payload"

        _ ->
            "Unknown error"


delay : Time.Time -> Msg -> Cmd Msg
delay t msg =
    Process.sleep t |> Task.perform (\_ -> msg)
