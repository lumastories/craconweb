module Update exposing (update)

import Api
import Empty
import Entity
import GenGame
import Html exposing (..)
import Html.Attributes exposing (..)
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
import VisualSearch


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

        SetStatus response ->
            let
                resp =
                    errorCodeEncoder response
            in
                ( { model | informing = Just resp.error }, Cmd.none )

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
                    { tmpUserRecord_old_ | roles = [ model.userRole.id ], groupId = Maybe.withDefault "" model.groupIdCon }

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

                        "exp" ->
                            { tmpUserRecord_old | groupId = value }

                        "con" ->
                            { tmpUserRecord_old | groupId = value }

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
                    ]
            in
                ( model, Cmd.batch cmds )

        OnUpdateLocation location ->
            ( { model
                | activeRoute = R.parseLocation location
                , isMenuActive = False
                , playingGame = Nothing
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
                | user = Just user_
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
                    { blocks = blocks
                    , currTime = currTime
                    , maxDuration = 5 * Time.minute
                    , settings = trialSettings
                    , instructionsView = ssInstructions
                    , trialRestView = Html.text ""
                    , trialRestDuration = 500 * Time.millisecond
                    , trialRestJitter = 0
                    , blockRestView = always (Html.text "Implement a block rest view.")
                    , blockRestDuration = 1500 * Time.millisecond
                    , reportView = always (Html.text "Implement a report view.")
                    , trialFuns = StopSignal.trialFuns
                    }

                getImages =
                    getFullImagePaths model.filesrv
            in
                applyImages StopSignal model gameSettings (\v i _ -> StopSignal.init trialSettings v i)

        -- TODO fetch configuration from the model
        InitGoNoGo ->
            let
                trialSettings =
                    { blockCount = 10000
                    , responseCount = 40
                    , nonResponseCount = 40
                    , fillerCount = 20
                    , pictureDuration = 1750 * Time.millisecond
                    , durationIncrement = -20 * Time.millisecond
                    , minDuration = 1250 * Time.millisecond
                    , maxDuration = 2000 * Time.millisecond
                    , redCross = 500 * Time.millisecond
                    }

                --<| Maybe.withDefault "" <| Maybe.map .instruct model.gonogoGame
                gameSettings blocks currTime =
                    { blocks = blocks
                    , currTime = currTime
                    , maxDuration = 5 * Time.minute
                    , settings = trialSettings
                    , instructionsView = gngInstructions
                    , trialRestView = Html.text ""
                    , trialRestDuration = 500 * Time.millisecond
                    , trialRestJitter = 0
                    , blockRestView = always (Html.text "Implement a block rest view.")
                    , blockRestDuration = 1500 * Time.millisecond
                    , reportView = always (Html.text "Implement a report view.")
                    , trialFuns = GoNoGo.trialFuns
                    }

                getImages =
                    getFullImagePaths model.filesrv
            in
                applyImages GoNoGo model gameSettings (GoNoGo.init trialSettings)

        -- TODO fetch configuration from the model
        InitDotProbe ->
            let
                trialSettings =
                    { blockCount = 10000
                    , fixationCross = 500 * Time.millisecond
                    , pictures = 500
                    }

                gameSettings blocks currTime =
                    { blocks = blocks
                    , currTime = currTime
                    , maxDuration = 5 * Time.minute
                    , settings = trialSettings
                    , instructionsView = Html.text "You will see pictures on the left and right side of the screen, followed by a dot on the left or right side of the screen. Press the \" c \" if the dot is on the left side of the screen or \" m \" when the dot is on the right side of the screen. Go as fast as you can, but don't sacrifice accuracy for speed. Press any key to continue."
                    , trialRestView = Html.text ""
                    , trialRestDuration = 0
                    , trialRestJitter = 0
                    , blockRestView = always (Html.text "Implement a block rest view.")
                    , blockRestDuration = 1500 * Time.millisecond
                    , reportView = always (Html.text "Implement a report view.")
                    , trialFuns = DotProbe.trialFuns
                    }
            in
                applyImages DotProbe model gameSettings (\v i _ -> DotProbe.init trialSettings v i)

        -- TODO fetch configuration from the model
        InitVisualSearch ->
            let
                trialSettings =
                    { picturesPerTrial = 16
                    , blockTrialCount = 10000
                    , fixationCross = 500
                    , selectionGrid = 3000
                    , animation = 1000
                    }

                gameSettings blocks currTime =
                    { blocks = blocks
                    , currTime = currTime
                    , maxDuration = 5 * Time.minute
                    , settings = trialSettings
                    , instructionsView = vsInstructions
                    , trialRestView = Html.text ""
                    , trialRestDuration = 0
                    , trialRestJitter = 0
                    , blockRestView = always (Html.text "Implement a block rest view.")
                    , blockRestDuration = 1500 * Time.millisecond
                    , reportView = always (Html.text "Implement a report view.")
                    , trialFuns = VisualSearch.trialFuns IntIndication
                    }
            in
                applyImages VisualSearch model gameSettings (\v i _ -> VisualSearch.init trialSettings v i)

        GameResp (Ok game) ->
            case game.slug of
                "gonogo" ->
                    ( { model | gonogoGame = Just game }, Cmd.none )

                "dotprobe" ->
                    ( { model | dotprobeGame = Just game }, Cmd.none )

                "stopsignal" ->
                    ( { model | stopsignalGame = Just game }, Cmd.none )

                "respondsignal" ->
                    ( { model | respondsignalGame = Just game }, Cmd.none )

                "visualsearch" ->
                    ( { model | visualsearchGame = Just game }, Cmd.none )

                _ ->
                    model ! []

        Presses keyCode ->
            let
                ( indModel, indCmd ) =
                    handleIndicationUpdate model

                ( keyModel, keyCmd ) =
                    case keyCode of
                        99 ->
                            handleDirectionIndicationUpdate GenGame.Left indModel

                        109 ->
                            handleDirectionIndicationUpdate GenGame.Right indModel

                        _ ->
                            ( indModel, Cmd.none )
            in
                ( keyModel, Cmd.batch [ indCmd, keyCmd ] )

        IntIndication n ->
            handleIntIndicationUpdate n model

        MainMenuToggle ->
            let
                active =
                    if model.isMenuActive then
                        False
                    else
                        True
            in
                ( { model | isMenuActive = active }, Cmd.none )

        NewCurrentTime t ->
            handleTimeUpdate t model

        RoleResp (Ok role) ->
            ( { model | userRole = role }, Cmd.none )

        FillerResp (Ok ugimages) ->
            ( { model | ugimages_f = Just ugimages }
            , preloadUgImages model.filesrv ugimages
            )

        ValidResp (Ok ugimages) ->
            ( { model | ugimages_v = Just ugimages }
            , preloadUgImages model.filesrv ugimages
            )

        InvalidResp (Ok ugimages) ->
            ( { model | ugimages_i = Just ugimages }
            , preloadUgImages model.filesrv ugimages
            )

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


applyImages :
    (GM.GameData settings trial Msg -> Game)
    -> Model
    -> (List (List trial) -> Time -> GM.InitConfig settings trial Msg)
    -> (List String -> List String -> List String -> Generator (List (List trial)))
    -> ( Model, Cmd Msg )
applyImages gameConstructor model gameSettings fun =
    let
        getImages =
            getFullImagePaths model.filesrv
    in
        Maybe.map3
            (\v i f ->
                ( model, handleGameInit gameConstructor (fun v i f) gameSettings )
            )
            (getImages model.ugimages_v)
            (getImages model.ugimages_i)
            (getImages model.ugimages_f)
            |> Maybe.withDefault ( model, Cmd.none )


getFullImagePaths : String -> Maybe (List Entity.Ugimage) -> Maybe (List String)
getFullImagePaths prefix =
    Maybe.map (List.filterMap .gimage >> List.map (.path >> (++) (prefix ++ "/repo/")))


preloadUgImages : String -> List Entity.Ugimage -> Cmd Msg
preloadUgImages prefix images =
    getFullImagePaths prefix (Just images)
        |> Maybe.withDefault []
        |> Port.preload


handleGameInit :
    (GM.GameData settings trial Msg -> Game)
    -> Generator (List (List trial))
    -> (List (List trial) -> Time -> GM.InitConfig settings trial Msg)
    -> Cmd Msg
handleGameInit gameConstructor blockGenerator gameF =
    Task.map2
        (\blocks currTime ->
            gameF blocks currTime
                |> GM.init
                |> GenGame.generatorToTask
        )
        (GenGame.generatorToTask blockGenerator)
        Time.now
        |> Task.andThen identity
        |> Task.perform (PlayGame << gameConstructor)


handleTimeUpdate : Time -> Model -> ( Model, Cmd Msg )
handleTimeUpdate time model =
    let
        updateData gameConstructor data =
            case GM.updateTime time data of
                ( GM.Running newData, cmd ) ->
                    ( { model | playingGame = Just (gameConstructor newData) }, cmd )

                ( GM.Results newData, cmd ) ->
                    ( { model | playingGame = Nothing }, cmd )
    in
        case model.playingGame of
            Nothing ->
                ( model, Cmd.none )

            Just (StopSignal data) ->
                updateData StopSignal data

            Just (GoNoGo data) ->
                updateData GoNoGo data

            Just (DotProbe data) ->
                updateData DotProbe data

            Just (VisualSearch data) ->
                updateData VisualSearch data


handleIndicationUpdate : Model -> ( Model, Cmd Msg )
handleIndicationUpdate model =
    let
        updateData gameConstructor data =
            case GM.updateIndication data of
                ( GM.Running newData, cmd ) ->
                    ( { model | playingGame = Just (gameConstructor newData) }, cmd )

                ( GM.Results newData, cmd ) ->
                    ( { model | playingGame = Nothing }, cmd )
    in
        case model.playingGame of
            Nothing ->
                ( model, Cmd.none )

            Just (StopSignal data) ->
                updateData StopSignal data

            Just (GoNoGo data) ->
                updateData GoNoGo data

            Just (DotProbe data) ->
                updateData DotProbe data

            Just (VisualSearch data) ->
                updateData VisualSearch data


handleIntIndicationUpdate : Int -> Model -> ( Model, Cmd Msg )
handleIntIndicationUpdate n model =
    let
        updateData gameConstructor data =
            case GM.updateIntIndication n data of
                ( GM.Running newData, cmd ) ->
                    ( { model | playingGame = Just (gameConstructor newData) }, cmd )

                ( GM.Results newData, cmd ) ->
                    ( { model | playingGame = Nothing }, cmd )
    in
        case model.playingGame of
            Nothing ->
                ( model, Cmd.none )

            Just (StopSignal data) ->
                updateData StopSignal data

            Just (GoNoGo data) ->
                updateData GoNoGo data

            Just (DotProbe data) ->
                updateData DotProbe data

            Just (VisualSearch data) ->
                updateData VisualSearch data


handleDirectionIndicationUpdate : GenGame.Direction -> Model -> ( Model, Cmd Msg )
handleDirectionIndicationUpdate n model =
    let
        updateData gameConstructor data =
            case GM.updateDirectionIndication n data of
                ( GM.Running newData, cmd ) ->
                    ( { model | playingGame = Just (gameConstructor newData) }, cmd )

                ( GM.Results newData, cmd ) ->
                    ( { model | playingGame = Nothing }, cmd )
    in
        case model.playingGame of
            Nothing ->
                ( model, Cmd.none )

            Just (StopSignal data) ->
                updateData StopSignal data

            Just (GoNoGo data) ->
                updateData GoNoGo data

            Just (DotProbe data) ->
                updateData DotProbe data

            Just (VisualSearch data) ->
                updateData VisualSearch data


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



-- Instruction Views


highlight : String -> Html Msg
highlight l =
    span [ class "highlight" ] [ strong [] [ text l ] ]


solid : String -> Html Msg
solid t =
    span [ style [ ( "border", "solid 1px #000" ), ( "padding", "2px" ) ] ] [ text t ]


dashed : String -> Html Msg
dashed t =
    span [ style [ ( "border", "dashed 1px #000" ), ( "padding", "2px" ) ] ] [ text t ]


base : List (Html Msg) -> Html Msg
base kids =
    div [ class "columns" ] [ div [ class "column is-half is-offset-one-quarter" ] [ div [ class "box" ] kids ] ]


title : Html Msg
title =
    h3 [ class "title" ] [ text "Instructions" ]


vsInstructions : Html Msg
vsInstructions =
    base [ title, text "You will see a grid of 16 images of food. It is your job to swipe on the image of the healthy food as quickly as you can. Press any key to continue." ]


rsInstructions : Html Msg
rsInstructions =
    base [ title, text "You will see pictures on the screen. Some of the pictures will be followed by a tone (a beep). Please press the space bar as quickly as you can. BUT only if you hear a beep after the picture. Do not press if you do not hear a beep." ]


ssInstructions : Html Msg
ssInstructions =
    base [ title, text "You will see pictures presented in either a dark blue or light gray border. Press the space bar as quickly as you can. BUT only if you see a blue border around the picture. Do not press if you see a grey border. Go as fast as you can, but don't sacrifice accuracy for speed. Press any key to continue." ]


gngInstructions : Html Msg
gngInstructions =
    base [ title, p [] [ text "You will see pictures either on the left or right side of the screen, surrounded by a solid or dashed border. Press ", highlight "c", text " when the picture is on the left side of the screen or ", highlight "m", text " when the picture is on the right side of the screen. BUT only if you see a ", solid "solid border", text " around the picture. Do not press if you see a ", dashed "dashed border", text ". Go as fast as you can, but don't sacrifice accuracy for speed.", br [] [], br [] [], strong [] [ text "Press any key to continue." ] ] ]
