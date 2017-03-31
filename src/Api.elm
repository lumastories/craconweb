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


fetchUser : String -> String -> String -> Http.Request Entity.User
fetchUser api token sub =
    Http.request
        { method = "GET"
        , headers = defaultHeaders token
        , url = api ++ "/user/" ++ sub
        , body = Http.emptyBody
        , expect = Http.expectJson Entity.userDecoder
        , timeout = Nothing
        , withCredentials = False
        }


fetchUsers : String -> String -> Http.Request (List Entity.User)
fetchUsers api token =
    Http.request
        { method = "GET"
        , headers = defaultHeaders token
        , url = api ++ "/users?createdEach=true"
        , body = Http.emptyBody
        , expect =
            Http.expectJson <|
                Decode.field "users" (Decode.list Entity.userDecoder)
        , timeout = Nothing
        , withCredentials = False
        }


createUser :
    String
    -> String
    -> Entity.UserRecord
    -> Http.Request Entity.UserRecord
createUser api token user =
    Http.request
        { method = "POST"
        , headers = defaultHeaders token
        , url = api ++ "/user/"
        , body = Http.jsonBody <| Entity.userRecordEncoder user
        , expect = Http.expectJson Entity.userRecordDecoder
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


getRole : String -> String -> String -> Http.Request Entity.Role
getRole api token slug =
    Http.request
        { method = "GET"
        , headers = defaultHeaders token
        , url = api ++ "/role/" ++ slug
        , body = Http.emptyBody
        , expect = Http.expectJson Entity.roleDecoder
        , timeout = Nothing
        , withCredentials = False
        }
