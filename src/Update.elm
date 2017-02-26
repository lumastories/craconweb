module Update exposing (update)

import Model exposing (..)
import Http
import Navigation
import Port
import Json.Decode as Decode
import Json.Decode.Pipeline as JP
import Json.Encode as Encode


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
            ( { model | jwttoken = newToken, spin = False, page = GamePage }, Cmd.batch [ Navigation.newUrl "#games", Port.jwtAuthSave newToken.token ] )

        LoginResponse (Err err) ->
            ( { model | spin = False, error = "Uh oh! Try again." }, Cmd.none )

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


authUrl : Model -> String
authUrl model =
    model.api ++ "/auth"


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
            userEncoder model |> Http.jsonBody

        url =
            authUrl model

        decoder =
            jwtDecoder
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


userEncoder : Model -> Encode.Value
userEncoder model =
    Encode.object
        [ ( "email", Encode.string model.email )
        , ( "password", Encode.string model.password )
        ]


jwtDecoder : Decode.Decoder JwtToken
jwtDecoder =
    JP.decode JwtToken
        |> JP.required "token" (Decode.string)
