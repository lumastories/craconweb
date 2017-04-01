module Api exposing (..)

import Entity
import Http
import Json.Decode as JD
import Json.Decode.Pipeline as JP
import Jwt
import Time


type alias JwtPayload =
    { aud : String
    , exp : Int
    , iat : Int
    , iss : String
    , sub : String
    , roles : List Entity.Role
    }


isOld : Time.Time -> String -> Bool
isOld now token =
    case jwtDecoded token of
        Ok decoded ->
            (toFloat decoded.exp) > (now / 1000)

        Err _ ->
            False


jwtDecoded : String -> Result Jwt.JwtError JwtPayload
jwtDecoded token =
    Jwt.decodeToken jwtPayloadDecoder token


jwtPayloadDecoder : JD.Decoder JwtPayload
jwtPayloadDecoder =
    JP.decode JwtPayload
        |> JP.required "aud" (JD.string)
        |> JP.required "exp" (JD.int)
        |> JP.required "iat" (JD.int)
        |> JP.required "iss" (JD.string)
        |> JP.required "sub" (JD.string)
        |> JP.required "roles" (JD.list Entity.roleDecoder)


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
                JD.field "users" (JD.list Entity.userDecoder)
        , timeout = Nothing
        , withCredentials = False
        }


createUser :
    String
    -> String
    -> Entity.UserRecord
    -> Http.Request Entity.User
createUser api token user =
    Http.request
        { method = "POST"
        , headers = defaultHeaders token
        , url = api ++ "/user"
        , body = Http.jsonBody <| Entity.userRecordEncoder user
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



-- server message decoders


type alias ErrorCode =
    { error : String
    , code : Int
    }


decodeErrorCode errorCode =
    case JD.decodeString errorCodeDecoder errorCode of
        Ok ed ->
            ed

        Err _ ->
            { error = "error"
            , code = 0
            }


errorCodeDecoder : JD.Decoder ErrorCode
errorCodeDecoder =
    JD.lazy <|
        \_ ->
            JD.succeed ErrorCode
                |> required "error" JD.string ""
                |> required "code" JD.int 0



-- Copied from Protobuf.elm


withDefault : a -> JD.Decoder a -> JD.Decoder a
withDefault default decoder =
    JD.oneOf
        [ decoder
        , JD.succeed default
        ]


required : String -> JD.Decoder a -> a -> JD.Decoder (a -> b) -> JD.Decoder b
required name decoder default d =
    JD.map2 (|>) (withDefault default <| JD.field name decoder) d
