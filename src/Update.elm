module Update exposing (update)

import Api
import Empty
import GenGame
import Html
import Http
import Model exposing (..)
import Navigation
import Navigation
import Port
import Process
import Random exposing (Generator)
import Routing as R
import Task exposing (Task)
import Time exposing (Time)


-- Game Modules

import GameManager as GM
import StopSignal
import GoNoGo
import DotProbe
import RespondSignal


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TryUpdateUser ->
            ( model
            , Port.upload
                ( model.tasksrv ++ "/upload/ugimgset"
                , "csvForm"
                , model.jwtencoded
                )
            )

        SetStatus message ->
            ( { model | informing = Just message }, Cmd.none )

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
            model ! []

        TryRegisterUser ->
            ( { model | loading = Just "loading..." }
            , Cmd.batch
                [ Task.attempt RegisterUserResp
                    (Api.createUserRecord
                        model.httpsrv
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
                ( { model | loading = Nothing, users = users_, tmpUserRecord = Empty.emptyUserRecord }, Navigation.newUrl R.adminPath )

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
                | activeRoute = R.parseLocation location
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
                    Task.attempt AuthResp
                        (Api.createAuthRecord
                            model.httpsrv
                            model.authRecord
                        )
            in
                ( { model | loading = Just "loading..." }, cmd )

        Logout ->
            let
                cmds =
                    [ Port.clear ()
                    , Navigation.newUrl "/login"
                    , Port.ping ()
                    ]
            in
                ( Empty.emptyModel model, Cmd.batch cmds )

        AuthResp (Ok auth) ->
            let
                jwtdecoded_ =
                    Api.jwtDecoded auth.token

                ( model_, command_ ) =
                    case jwtdecoded_ of
                        Ok jwt ->
                            ( { model
                                | loading = Nothing
                                , visitor = LoggedIn jwt
                                , jwtencoded = auth.token
                                , glitching = Nothing
                              }
                            , Cmd.batch
                                [ Port.set ( "token", tokenEncoder auth.token )
                                , Api.fetchAll model.httpsrv jwt auth.token
                                , Navigation.newUrl R.homePath
                                ]
                            )

                        Err err ->
                            ( { model
                                | loading = Nothing
                                , glitching = Just (toString err)
                              }
                            , Cmd.none
                            )
            in
                ( model_, command_ )

        UserResp (Ok user_) ->
            ( { model
                | user = user_
              }
            , Cmd.none
            )

        PlayGame game ->
            ( { model | playingGame = Just game }, Cmd.none )

        StopGame ->
            ( { model | playingGame = Nothing }, Cmd.none )

        -- TODO fetch configuration from the model
        InitStopSignal ->
            let
                trialSettings =
                    { blockResponseCount = 20
                    , blockNonResponseCount = 20
                    , pictureNoBorder = 100 * Time.millisecond
                    , pictureBorder = 900 * Time.millisecond
                    , redCross = 500 * Time.millisecond
                    }

                gameSettings blocks currTime =
                    { gameConstructor = GM.StopSignal
                    , blocks = blocks
                    , currTime = currTime
                    , settings = trialSettings
                    , instructionsView = StopSignal.instructions
                    , instructionsDuration = 10 * Time.second
                    , trialRestView = Html.text ""
                    , trialRestDuration = 500 * Time.millisecond
                    , blockRestView = StopSignal.blockRestView
                    , blockRestDuration = 1500 * Time.millisecond
                    }

                getImages =
                    getFullImagePaths model.filesrv
            in
                Maybe.map2
                    (\v i ->
                        ( model
                        , handleGameInit (StopSignal.init trialSettings v i) gameSettings
                        )
                    )
                    (getImages model.validImages)
                    (getImages model.invalidImages)
                    |> Maybe.withDefault
                        ( model, Cmd.none )

        -- TODO fetch configuration from the model
        InitGoNoGo ->
            let
                trialSettings =
                    { blockCount = 10000
                    , responseCount = 40
                    , nonResponseCount = 40
                    , fillerCount = 20
                    , picture = 1250 * Time.millisecond
                    , redCross = 500 * Time.millisecond
                    }

                gameSettings blocks currTime =
                    { gameConstructor = GM.GoNoGo
                    , blocks = blocks
                    , currTime = currTime
                    , settings = trialSettings
                    , instructionsView = GoNoGo.instructions
                    , instructionsDuration = 10 * Time.second
                    , trialRestView = Html.text ""
                    , trialRestDuration = 500 * Time.millisecond
                    , blockRestView = GoNoGo.blockRestView
                    , blockRestDuration = 1500 * Time.millisecond
                    }

                getImages =
                    getFullImagePaths model.filesrv
            in
                Maybe.map3
                    (\v i f ->
                        ( model
                        , handleGameInit (GoNoGo.init trialSettings v i f) gameSettings
                        )
                    )
                    (getImages model.validImages)
                    (getImages model.invalidImages)
                    (getImages model.fillerImages)
                    |> Maybe.withDefault ( model, Cmd.none )

        -- TODO fetch configuration from the model
        InitRespondSignal ->
            let
                trialSettings =
                    { totalPictureTime = 100 * Time.millisecond
                    , feedback = 500
                    , delayMin = 200
                    , delayMax = 400
                    , blockTrialCount = 44
                    , responseCount = 80
                    , nonResponseCount = 80
                    , fillerCount = 32
                    , audioEvent =
                        Cmd.none
                        -- TODO use audio signal port
                    }

                gameSettings blocks currTime =
                    { gameConstructor = GM.RespondSignal
                    , blocks = blocks
                    , currTime = currTime
                    , settings = trialSettings
                    , instructionsView = RespondSignal.instructions
                    , instructionsDuration = 10 * Time.second
                    , trialRestView = Html.text ""
                    , trialRestDuration = 0
                    , blockRestView = RespondSignal.blockRestView
                    , blockRestDuration = 1500 * Time.millisecond
                    }
            in
                applyImages model gameSettings (RespondSignal.init trialSettings)

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
            handleGameUpdate (GM.updateTime t) { model | currentTime = t }

        IntIndication i ->
            handleGameUpdate (GM.updateIntIndication i) model

        StartGameWith time ->
            ( { model
                | startTime = time
              }
            , Cmd.none
            )

        RoleResp (Ok role) ->
            ( { model | userRole = role }, Cmd.none )

        FillerResp (Ok ugimages) ->
            ( { model | fillerImages = Just ugimages }, Cmd.none )

        ValidResp (Ok ugimages) ->
            ( { model | validImages = Just ugimages }, Cmd.none )

        InvalidResp (Ok ugimages) ->
            ( { model | invalidImages = Just ugimages }, Cmd.none )

        FillerResp (Err err) ->
            (valuationsErrState model err)

        ValidResp (Err err) ->
            (valuationsErrState model err)

        InvalidResp (Err err) ->
            (valuationsErrState model err)

        AuthResp (Err err) ->
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



--comment


getFullImagePaths prefix =
    Maybe.map (List.filterMap .gimage >> List.map (.path >> (++) (prefix ++ "/repo/")))


handleGameInit :
    Generator (List (List trial))
    -> (List (List trial) -> Time -> GM.InitConfig settings trial Msg)
    -> Cmd Msg
handleGameInit blockGenerator gameF =
    blockGenerator
        |> GenGame.generatorToTask
        |> Task.andThen
            (\blocks ->
                Time.now
                    |> Task.map
                        (\currTime ->
                            gameF blocks currTime
                                |> GM.init
                        )
            )
        |> Task.perform PlayGame


handleGameUpdate : (GM.Game Msg -> ( GM.GameStatus Msg, Cmd Msg )) -> Model -> ( Model, Cmd Msg )
handleGameUpdate f model =
    case model.playingGame of
        Nothing ->
            ( model, Cmd.none )

        Just game ->
            case f game of
                ( GM.Running newGame, cmd ) ->
                    ( { model | playingGame = Just newGame }, cmd )

                ( GM.Results newGame, cmd ) ->
                    ( { model | playingGame = Nothing }, cmd )


isAdmin : Visitor -> Bool
isAdmin visitor =
    case visitor of
        LoggedIn jwt ->
            List.map .name jwt.roles
                |> List.member "admin"

        _ ->
            False


valuationsErrState : Model -> ValuationsError -> ( Model, Cmd msg )
valuationsErrState model err =
    ( { model
        | loading = Nothing
        , glitching = Just (valuationsError err)
        , httpErr = toString err
      }
    , Cmd.none
    )


httpErrorState : Model -> Http.Error -> ( Model, Cmd msg )
httpErrorState model err =
    ( { model
        | loading = Nothing
        , glitching = Just (httpHumanError err)
        , httpErr = toString err
      }
    , Cmd.none
    )


valuationsError : ValuationsError -> String
valuationsError err =
    case err of
        ReqFail httpErr ->
            httpHumanError httpErr

        MissingValuations ->
            "You are missing customized game images! Are your image valuations uploaded?"


httpHumanError : Http.Error -> String
httpHumanError err =
    case err of
        Http.Timeout ->
            "Something is taking too long"

        Http.NetworkError ->
            "Oops. There's been a network error."

        Http.BadStatus s ->
            "Server error: " ++ (.error (errorCodeEncoder s.body))

        Http.BadPayload str _ ->
            "Bad payload"

        _ ->
            "Unknown error"


delay : Time.Time -> Msg -> Cmd Msg
delay t msg =
    Process.sleep t |> Task.perform (\_ -> msg)
