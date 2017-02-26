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
        UpdateEmail newEmail ->
            ( { model | email = newEmail }, Cmd.none )

        UpdatePassword newPassword ->
            ( { model | password = newPassword }, Cmd.none )

        ChangePage location ->
            let
                newPage =
                    page location.hash
            in
                ( { model | history = location :: model.history, page = newPage }, Cmd.none )

        Login ->
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
                hitEnter =
                    case code of
                        '\x0D' ->
                            True

                        _ ->
                            False

                cmd =
                    case hitEnter of
                        True ->
                            if model.page == LoginPage then
                                Http.send LoginResponse (postCreds model)
                            else
                                Cmd.none

                        False ->
                            Cmd.none
            in
                ( { model | presses = code :: model.presses, spin = hitEnter }, cmd )


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
