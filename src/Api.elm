module Api exposing (..)

import Model exposing (..)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as JP
import Json.Encode as Encode

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
