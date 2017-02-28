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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Routing
        ChangeLocation path ->
            ( { model | changes = model.changes + 1 }, Navigation.newUrl path )

        OnLocationChange location ->
            let
                newRoute =
                    parseLocation location
            in
                ( { model | activeRoute = newRoute }, Cmd.none )

        -- Actions
        ChangeEmail newEmail ->
            ( { model | email = newEmail }, Cmd.none )

        ChangePassword newPassword ->
            ( { model | password = newPassword }, Cmd.none )

        TryLogin ->
            let
                cmd =
                    Http.send LoginResponse (postCreds model)
            in
                ( { model | spin = True }, cmd )

        -- HTTP Responses
        LoginResponse (Ok newToken) ->
            let
                commands =
                    [ (Http.send UserResponse (getUser model))
                    , Port.setItem ( "token", newToken )
                    ]
            in
                ( { model | jwtencoded = newToken, spin = False }, Cmd.batch commands )

        LoginResponse (Err err) ->
            ( { model | spin = False, error = "Uh oh! Try again." }, Cmd.none )

        UserResponse (Ok newUser) ->
            let
                cmds =
                    [ Port.setItem ( "firstName", newUser.firstName )
                    , Navigation.newUrl "/"
                    ]
            in
                ( { model | user = newUser, activeRoute = HomeRoute }, Cmd.batch cmds )

        UserResponse (Err err) ->
            ( { model | error = "Uh oh! User error." }, Cmd.none )

        Presses _ ->
            model ! []



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
            , headers = defaultHeaders model
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

        sub =
            Result.map .sub model.jwtdecoded

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
