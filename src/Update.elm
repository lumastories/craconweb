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
import Thing
import Auth
import Jwt


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Routing
        UpdateLocation path ->
            ( { model | changes = model.changes + 1 }, Navigation.newUrl path )

        OnUpdateLocation location ->
            let
                newRoute =
                    parseLocation location
            in
                ( { model | activeRoute = newRoute, menuIsActive = False }, Cmd.none )

        -- Actions
        UpdateEmail newEmail ->
            ( { model | email = newEmail }, Cmd.none )

        UpdatePassword newPassword ->
            ( { model | password = newPassword }, Cmd.none )

        TryLogin ->
            let
                cmd =
                    Http.send LoginResponse (postCreds model)
            in
                ( { model | spin = True }, cmd )

        -- HTTP Responses
        LoginResponse (Ok newJwtencoded) ->
            let
                commands =
                    [ Port.setItem ( "token", newJwtencoded )
                    , (Http.send UserResponse (getUser model))
                    ]

                newJwtdecoded =
                    Jwt.decodeToken Auth.decodeJwtPayload newJwtencoded
            in
                ( { model | jwtencoded = newJwtencoded, spin = False }, Cmd.batch commands )

        LoginResponse (Err err) ->
            ( { model | spin = False, error = (toString err) }, Cmd.none )

        UserResponse (Ok newUser) ->
            let
                commands =
                    [ Port.setItem ( "firstName", newUser.firstName )
                    , Navigation.newUrl "/"
                    ]
            in
                ( { model | user = newUser, activeRoute = HomeRoute }, Cmd.batch commands )

        UserResponse (Err err) ->
            ( { model | error = (toString err) }, Cmd.none )

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
                ( initialModel emptyFlags emptyLocation, Cmd.batch cmds )


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



-- TODO decode json, ask for human friendly errors? Or Not
-- TODO this should probably only happen in development, otherwise
-- process error through a case and return helpful errors for the user.


humanizeErr err =
    toString err



-- Application Programing Interface
-- Uniform Resource Locators
-- REpresentational State Transfer
-- IOW, talk to Daved's code :)


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


postCreds : Model -> Http.Request String
postCreds model =
    let
        body =
            encodeCreds model |> Http.jsonBody

        url =
            model.api ++ "/auth"

        decoder : Json.Decode.Decoder String
        decoder =
            Json.Decode.field "token" (Json.Decode.string)
    in
        Http.request
            { method = "POST"
            , headers = []
            , url = url
            , body = body
            , expect = Http.expectJson decoder
            , timeout = Nothing
            , withCredentials = False
            }


getUser : Model -> Http.Request Thing.User
getUser model =
    let
        body =
            encodeCreds model |> Http.jsonBody

        users =
            model.api ++ "/user/"

        newJwtdecoded =
            (Jwt.decodeToken Auth.decodeJwtPayload model.jwtencoded)

        sub =
            Result.map .sub newJwtdecoded

        url =
            case sub of
                Ok userId ->
                    users ++ userId

                Err _ ->
                    ""

        -- TODO send an error Http.send
        decoder =
            decodeUser
    in
        Http.request
            { method = "GET"
            , headers = defaultHeaders model
            , url = url
            , body = body
            , expect = Http.expectJson decoder
            , timeout = Nothing
            , withCredentials = False
            }



-- Decoders and Encoders


encodeCreds : Model -> Json.Encode.Value
encodeCreds model =
    Json.Encode.object
        [ ( "email", Json.Encode.string model.email )
        , ( "password", Json.Encode.string model.password )
        ]


decodeUser : Json.Decode.Decoder Thing.User
decodeUser =
    JP.decode Thing.User
        |> JP.required "id" (Json.Decode.string)
        |> JP.required "username" (Json.Decode.string)
        |> JP.required "email" (Json.Decode.string)
        |> JP.required "firstName" (Json.Decode.string)
        |> JP.required "lastName" (Json.Decode.string)
