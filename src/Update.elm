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
import Auth
import Jwt
import Time
import Task
import Entity
import Process


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
                ( { model | changes = model.changes + 1 }, Cmd.batch cmds )

        VerifyToken now ->
            let
                jwtExpired =
                    Jwt.isExpired now model.jwtencoded

                jwtExpiredCmd =
                    case jwtExpired of
                        Ok _ ->
                            Cmd.none

                        -- TODO Logout!
                        Err _ ->
                            Cmd.none
            in
                ( model, jwtExpiredCmd )

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
                    Http.send LoginResponse (postCreds model)
            in
                ( { model | spin = True }, cmd )

        -- HTTP Responses
        LoginResponse (Ok auth) ->
            let
                commands =
                    [ Port.setItem ( "token", auth.token )
                    , (Http.send UserResponse (getUser model))
                    ]

                newJwtdecoded =
                    Jwt.decodeToken Auth.decodeJwtPayload auth.token
            in
                ( { model | jwtencoded = auth.token, spin = False }, Cmd.batch commands )

        LoginResponse (Err err) ->
            let
                l =
                    Debug.log "login" (toString err)
            in
                ( { model | spin = False, error = "Login related error" }, Cmd.none )

        UserResponse (Ok newUser) ->
            let
                commands =
                    [ Port.setItem ( "firstName", newUser.firstName )
                    , Navigation.newUrl "/"
                    ]
            in
                ( { model | user = newUser, activeRoute = HomeRoute }, Cmd.batch commands )

        UserResponse (Err err) ->
            let
                l =
                    Debug.log "login" (toString err)
            in
                ( { model | error = "User related error" }, Cmd.none )

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

                emptyFlags =
                    { token = ""
                    , firstName = ""
                    }
            in
                ( initModel emptyFlags emptyLocation, Cmd.batch cmds )

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


emptyLocation : Navigation.Location
emptyLocation =
    { href = ""
    , host = ""
    , hostname = ""
    , protocol = ""
    , origin = ""
    , port_ = ""
    , pathname = ""
    , search = ""
    , hash = ""
    , username = ""
    , password = ""
    }


defaultHeaders : Model -> List Http.Header
defaultHeaders model =
    let
        headers =
            [ Http.header "Accept" "application/json" ]

        authHeaders =
            case model.jwtencoded of
                "" ->
                    headers

                _ ->
                    (Http.header "Authorization" ("Bearer " ++ model.jwtencoded)) :: headers
    in
        authHeaders


postCreds : Model -> Http.Request Entity.Auth
postCreds model =
    Http.request
        { method = "POST"
        , headers = []
        , url = (model.api ++ "/auth")
        , body = (Entity.authRecordEncoder model.authRecord |> Http.jsonBody)
        , expect = Http.expectJson Entity.authDecoder
        , timeout = Nothing
        , withCredentials = False
        }

getGame : Model -> String -> Http.Request Entity.Game
getGame model slug =
        Http.request
            { method = "GET"
            , headers = defaultHeaders model
            , url = model.api ++ "/game/" ++ slug
            , body = (Entity.authRecordEncoder model.authRecord |> Http.jsonBody)
            , expect = Http.expectJson Entity.gameDecoder
            , timeout = Nothing
            , withCredentials = False
            }
    
getUser : Model -> Http.Request Entity.User
getUser model =
    let
        newJwtdecoded =
            (Jwt.decodeToken Auth.decodeJwtPayload model.jwtencoded)

        url =
            case (Result.map .sub newJwtdecoded) of
                Ok userId ->
                    model.api ++ "/user/" ++ userId
                Err _ ->
                    ""
    in
        Http.request
            { method = "GET"
            , headers = defaultHeaders model
            , url = url
            , body = (Entity.authRecordEncoder model.authRecord |> Http.jsonBody)
            , expect = Http.expectJson Entity.userDecoder
            , timeout = Nothing
            , withCredentials = False
            }
