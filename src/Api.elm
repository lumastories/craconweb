module Api exposing (..)

import Model exposing (..)
import Entity
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as JP
import Jwt


jwtDecoded token =
    Jwt.decodeToken decodeJwtPayload token


decodeJwtPayload : Decode.Decoder JwtPayload
decodeJwtPayload =
    JP.decode JwtPayload
        |> JP.required "aud" (Decode.string)
        |> JP.required "exp" (Decode.int)
        |> JP.required "iat" (Decode.int)
        |> JP.required "iss" (Decode.string)
        |> JP.required "sub" (Decode.string)
        |> JP.required "roles" (Decode.list Entity.roleDecoder)


defaultHeaders : String -> List Http.Header
defaultHeaders jwtencoded =
    let
        headers =
            [ Http.header "Accept" "application/json" ]

        authHeaders =
            case jwtencoded of
                "" ->
                    headers

                _ ->
                    (Http.header "Authorization" ("Bearer " ++ jwtencoded)) :: headers
    in
        authHeaders


postCreds : Model.Model -> Http.Request Entity.Auth
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


getGame : String -> String -> String -> Http.Request Entity.Game
getGame api token slug =
    Http.request
        { method = "GET"
        , headers = defaultHeaders token
        , url = api ++ "/game/" ++ slug
        , body = Http.emptyBody
        , expect = Http.expectJson Entity.gameDecoder
        , timeout = Nothing
        , withCredentials = False
        }


getUser : String -> String -> String -> Http.Request Entity.User
getUser api token sub =
    Http.request
        { method = "GET"
        , headers = defaultHeaders token
        , url = api ++ "/user/" ++ sub
        , body = Http.emptyBody
        , expect = Http.expectJson Entity.userDecoder
        , timeout = Nothing
        , withCredentials = False
        }


initData : String -> String -> List (Cmd Msg)
initData api token =
    [ Http.send GameResponse (getGame api token "gonogo")
    , Http.send GameResponse (getGame api token "dotprobe")
    , Http.send GameResponse (getGame api token "stopsignal")
    , Http.send GameResponse (getGame api token "respondsignal")
    , Http.send GameResponse (getGame api token "visualsearch")
    ]
