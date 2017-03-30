module Api exposing (..)

import Entity
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as JP
import Jwt


type alias JwtPayload =
    { aud : String
    , exp : Int
    , iat : Int
    , iss : String
    , sub : String
    , roles : List Entity.Role
    }


jwtDecoded : String -> Result Jwt.JwtError JwtPayload
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


postCreds : String -> Entity.AuthRecord -> Http.Request Entity.Auth
postCreds api authRecord =
    Http.request
        { method = "POST"
        , headers = []
        , url = api ++ "/auth"
        , body = (Entity.authRecordEncoder authRecord |> Http.jsonBody)
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


getUsers : String -> String -> Http.Request UsersList
getUsers api token =
    Http.request
        { method = "GET"
        , headers = defaultHeaders token
        , url = api ++ "/users?createdEach=true"
        , body = Http.emptyBody
        , expect = Http.expectJson usersListDecoder
        , timeout = Nothing
        , withCredentials = False
        }


postUser : String -> String -> Entity.User -> Http.Request Entity.User
postUser api token user =
    Http.request
        { method = "POST"
        , headers = defaultHeaders token
        , url = api ++ "/user/"
        , body = Http.jsonBody <| Entity.userEncoder user
        , expect = Http.expectJson Entity.userDecoder
        , timeout = Nothing
        , withCredentials = False
        }


getGroup : String -> String -> String -> Http.Request Entity.Group
getGroup api token slug =
    Http.request
        { method = "GET"
        , headers = defaultHeaders token
        , url = api ++ "/group/" ++ slug
        , body = Http.emptyBody
        , expect = Http.expectJson Entity.groupDecoder
        , timeout = Nothing
        , withCredentials = False
        }


usersListDecoder =
    JP.decode UsersList |> JP.required "users" (Decode.list Entity.userDecoder)


type alias UsersList =
    { users : List Entity.User
    }
