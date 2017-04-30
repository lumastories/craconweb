module Update exposing (update)

import Api
import Empty
import Entity
import Game
import Game.Card
import Game.Implementations
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
            handleInput Game.Initialize { model | playingGame = Just game }

        StopGame ->
            ( { model | playingGame = Nothing }, Cmd.none )

        -- TODO fetch configuration from the model
        InitStopSignal ->
            ( model
            , Time.now
                |> Task.map
                    (\time ->
                        Game.Implementations.stopSignalInit
                            { borderDelay = 100 * Time.millisecond
                            , totalDuration = 1000 * Time.millisecond
                            , infoString = "Placeholder"
                            , responseImages = (getFullImagePaths model.filesrv model.ugimages_v |> Maybe.withDefault [])
                            , nonResponseImages = (getFullImagePaths model.filesrv model.ugimages_i |> Maybe.withDefault [])
                            , seedInt = 0
                            , currentTime = time
                            , gameDuration = 0.1 * Time.minute
                            }
                    )
                |> Task.perform PlayGame
            )

        -- TODO fetch configuration from the model
        InitGoNoGo ->
            ( model, Cmd.none )

        -- TODO fetch configuration from the model
        InitDotProbe ->
            ( model, Cmd.none )

        -- TODO fetch configuration from the model
        InitRespondSignal ->
            ( model, Cmd.none )

        InitVisualSearch ->
            ( model, Cmd.none )

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
                ( newModel1, cmd1 ) =
                    handleInput Game.Indication model

                ( newModel2, cmd2 ) =
                    case keyCode of
                        99 ->
                            handleInput (Game.Direction Game.Left) newModel1

                        109 ->
                            handleInput (Game.Direction Game.Right) newModel1

                        _ ->
                            ( newModel1, Cmd.none )
            in
                ( newModel2, Cmd.batch [ cmd1, cmd2 ] )

        IntIndication n ->
            handleInput (Game.Select n) model

        NewCurrentTime t ->
            handleInput (Game.Tick t) model

        MainMenuToggle ->
            let
                active =
                    if model.isMenuActive then
                        False
                    else
                        True
            in
                ( { model | isMenuActive = active }, Cmd.none )

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


handleInput : Game.Input -> Model -> ( Model, Cmd Msg )
handleInput input model =
    case model.playingGame of
        Nothing ->
            ( model, Cmd.none )

        Just game ->
            case Game.Card.step input game of
                ( Game.Card.Complete state, cmd ) ->
                    ( { model | playingGame = Nothing }, cmd )

                ( Game.Card.Continue _ newGame, cmd ) ->
                    ( { model | playingGame = Just newGame }, cmd )


generatorToTask : Generator a -> Task x a
generatorToTask generator =
    Time.now
        |> Task.map (round >> Random.initialSeed >> Random.step generator >> Tuple.first)


getFullImagePaths : String -> Maybe (List Entity.Ugimage) -> Maybe (List Game.Image)
getFullImagePaths prefix =
    Maybe.map
        (List.filterMap .gimage
            >> List.map
                (\gimage ->
                    { url = prefix ++ "/repo/" ++ gimage.path
                    , id = gimage.id
                    }
                )
        )


preloadUgImages : String -> List Entity.Ugimage -> Cmd Msg
preloadUgImages prefix images =
    getFullImagePaths prefix (Just images)
        |> Maybe.withDefault []
        |> List.map .url
        |> Port.preload


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


highlight l =
    span [ class "highlight" ] [ strong [] [ text l ] ]


solid t =
    span [ style [ ( "border", "solid 1px #000" ), ( "padding", "2px" ) ] ] [ text t ]


dashed t =
    span [ style [ ( "border", "dashed 1px #000" ), ( "padding", "2px" ) ] ] [ text t ]


base kids =
    div [ class "columns" ] [ div [ class "column is-half is-offset-one-quarter" ] [ div [ class "box" ] kids ] ]


title =
    h3 [ class "title" ] [ text "Instructions" ]


vsInstructions =
    base [ title, text "You will see a grid of 16 images of food. It is your job to swipe on the image of the healthy food as quickly as you can. Press any key to continue." ]


rsInstructions : Html msg
rsInstructions =
    base [ title, text "You will see pictures on the screen. Some of the pictures will be followed by a tone (a beep). Please press the space bar as quickly as you can. BUT only if you hear a beep after the picture. Do not press if you do not hear a beep." ]


ssInstructions : Html msg
ssInstructions =
    base [ title, text "You will see pictures presented in either a dark blue or light gray border. Press the space bar as quickly as you can. BUT only if you see a blue border around the picture. Do not press if you see a grey border. Go as fast as you can, but don't sacrifice accuracy for speed. Press any key to continue." ]


gngInstructions : Html msg
gngInstructions =
    base [ title, p [] [ text "You will see pictures either on the left or right side of the screen, surrounded by a solid or dashed border. Press ", highlight "c", text " when the picture is on the left side of the screen or ", highlight "m", text " when the picture is on the right side of the screen. BUT only if you see a ", solid "solid border", text " around the picture. Do not press if you see a ", dashed "dashed border", text ". Go as fast as you can, but don't sacrifice accuracy for speed.", br [] [], br [] [], strong [] [ text "Press any key to continue." ] ] ]
