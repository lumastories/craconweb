module Update exposing (update)

import Model exposing (..)
import Http
import Navigation
import Port
import Json.Encode
import Json.Decode
import Json.Decode.Pipeline as JP
import Jwt


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoginEmail newEmail ->
            ( { model | email = newEmail }, Cmd.none )

        LoginPassword newPassword ->
            ( { model | password = newPassword }, Cmd.none )

        TryLogin ->
            let
                cmd =
                    Http.send LoginResponse (postCreds model)
            in
                ( { model | spin = True }, cmd )

        LoginResponse (Ok newToken) ->
            let
                commands =
                    [ (Http.send UserResponse (getUser model))
                    , Port.setJwt newToken
                    , Navigation.newUrl "#games"
                    ]
            in
                ( { model | jwtencoded = newToken, spin = False, activePage = Games }, Cmd.batch commands )

        -- TODO Why is this glitching out even with good creds?
        -- FIXED temporary - token had expired! need to check locally `(Jwt.isExpired time token) returns Result Bool`
        LoginResponse (Err err) ->
            ( { model | spin = False, error = "Uh oh! Try again." }, Cmd.none )

        UserResponse (Ok newUser) ->
            ( { model | user = newUser }, Cmd.none )

        UserResponse (Err err) ->
            ( { model | error = "Uh oh! User error." }, Cmd.none )

        SetActivePage page ->
            -- TODO, check if token is valid
            -- { model | activePage = setActivePageAccess True page } ! []
            model ! []

        CheckTokenExpiry now ->
            let
                tokenExpired =
                    case (Jwt.isExpired now model.jwtencoded) of
                        Ok _ ->
                            False

                        Err _ ->
                            True
            in
                model ! [ Port.removeJwt True ]

        Presses _ ->
            model ! []



-- TODO better naming conventions for functions
-- TODO use this function


setActivePageAccess : Bool -> Page -> Page
setActivePageAccess validToken desiredPage =
    case validToken of
        True ->
            if desiredPage == Login then
                AccessDenied
            else
                desiredPage

        False ->
            if desiredPage == Games then
                AccessDenied
            else
                desiredPage



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


getUser : Model -> Http.Request User
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


decodeUser : Json.Decode.Decoder User
decodeUser =
    JP.decode User
        |> JP.required "id" (Json.Decode.string)
        |> JP.required "username" (Json.Decode.string)
        |> JP.required "email" (Json.Decode.string)
        |> JP.required "firstName" (Json.Decode.string)
        |> JP.required "lastName" (Json.Decode.string)
