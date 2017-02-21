module Api exposing (..)

import Model exposing (..)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as JP


--import Html.Events exposing (..)

import Json.Encode as Encode


-- Application Programing Interface
-- Uniform Resource Locators
-- REpresentational State Transfer
-- IOW, talk to Daved's code :)


authUrl : Model -> String
authUrl model =
    model.api ++ "/auth"


defaultHeaders : List Http.Header
defaultHeaders = 
    [ Http.header "Access-Control-Allow-Origin" "localhost"
    , Http.header "Accept" "application/json"
    --, Http.header "Accept-Encoding" "gzip, deflate, br"
    , Http.header "Accept-Language" "en-US,en;q=0.8"
    --, Http.header "Connection" "keep-alive"
    , Http.header "Content-Type" "application/json"
    --, Http.header "Cookie" "csrftoken=P5XEJBgoA1kkOPE8d1bUjQXLeoE0mbwN; djdt=show"
    --, Http.header "Host" "127.0.0.1:8680"
    --, Http.header "Origin" "http://127.0.0.1:8680"
    ]


postCreds : Model -> Http.Request JwtToken
postCreds model =
    let

        body =
            userEncoder model |> Http.jsonBody
        
        l =
            Debug.log (toString <| userEncoder model) "user encoder"

        url =
            authUrl model

        decoder =
            jwtDecoder
    in
        Http.request
            { method = "POST"
            , headers = defaultHeaders
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
