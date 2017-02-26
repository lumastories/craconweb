module Update exposing (update)

import Model exposing (..)
import Http
import Navigation
import Port
import Json.Encode
import Json.Decode
import Json.Decode.Pipeline as JP


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoginEmail newEmail ->
            ( { model | email = newEmail }, Cmd.none )

        LoginPassword newPassword ->
            ( { model | password = newPassword }, Cmd.none )

        LoginSend ->
            let
                cmd =
                    Http.send LoginResponse (postCreds model)
            in
                ( { model | spin = True }, cmd )

        LoginResponse (Ok newToken) ->
            let
                commands =
                    [ (Http.send UserResponse (getUser model))
                    , Navigation.newUrl "#games"
                    , Port.jwtAuthSave newToken.token
                    ]
            in
                ( { model | jwttoken = newToken, spin = False, page = GamePage }, Cmd.batch commands )

        -- TODO Why is this glitching out even with good creds?
        LoginResponse (Err err) ->
            ( { model | spin = False, error = "Uh oh! Try again." }, Cmd.none )

        UserResponse (Ok newUser) ->
            ( model, Cmd.none )

        UserResponse (Err err) ->
            ( model, Cmd.none )

        Presses code ->
            let
                ( loading, cmd ) =
                    if code == '\x0D' then
                        cmdForEnterOnPage model
                    else
                        ( False, Cmd.none )
            in
                ( { model | spin = loading }, cmd )

        ChangePage location ->
            let
                -- TODO change page to LoginPage if Token is bad
                newPage =
                    page location.hash
            in
                ( { model | history = location :: model.history, page = newPage }, Cmd.none )



-- TODO better naming conventions for functions


cmdForEnterOnPage : Model -> ( Bool, Cmd Msg )
cmdForEnterOnPage model =
    case model.page of
        LoginPage ->
            ( True, Http.send LoginResponse (postCreds model) )

        _ ->
            ( False, Cmd.none )


page : String -> Page
page hash =
    case hash of
        "#games" ->
            GamePage

        "#badges" ->
            BadgePage

        _ ->
            LoginPage



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
            case model.jwttoken.token of
                "" ->
                    headers

                _ ->
                    (Http.header "Authorization" ("Bearer " ++ model.jwttoken.token)) :: headers
    in
        authHeaders


postCreds : Model -> Http.Request JwtToken
postCreds model =
    let
        body =
            encodeCreds model |> Http.jsonBody

        url =
            model.api ++ "/auth"

        decoder =
            decodeJwtToken
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

        -- TODO: remove magic :id, get from model.jwttoken.sub
        -- Maybe like this?
        -- url = model.jwttoken.map \token -> model.api ++ "/user/" ++ token.sub
        url =
            model.api ++ "/user/7239373074254188773"

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



-- Decode and Encode


encodeCreds : Model -> Json.Encode.Value
encodeCreds model =
    Json.Encode.object
        [ ( "email", Json.Encode.string model.email )
        , ( "password", Json.Encode.string model.password )
        ]


decodeJwtToken : Json.Decode.Decoder JwtToken
decodeJwtToken =
    JP.decode JwtToken
        |> JP.required "token" (Json.Decode.string)


decodeUser : Json.Decode.Decoder User
decodeUser =
    JP.decode User
        |> JP.required "id" (Json.Decode.string)
        |> JP.required "username" (Json.Decode.string)
        |> JP.required "email" (Json.Decode.string)
        |> JP.required "firstName" (Json.Decode.string)
        |> JP.required "lastName" (Json.Decode.string)


encodeUser : User -> Json.Encode.Value
encodeUser record =
    Json.Encode.object
        [ ( "id", Json.Encode.string <| record.id )
        , ( "username", Json.Encode.string <| record.username )
        , ( "email", Json.Encode.string <| record.email )
        , ( "firstName", Json.Encode.string <| record.firstName )
        , ( "lastName", Json.Encode.string <| record.lastName )
        ]
