module Api exposing (..)

import Entity
import Http
import Json.Decode as JD
import Json.Encode as JE
import Json.Decode.Pipeline as JP
import Jwt
import Time
import Protobuf exposing (..)


type alias CsvData =
    { upload : String
    , userid : String
    }


type alias JwtPayload =
    { aud : String
    , exp : Float
    , iat : Int
    , iss : String
    , sub : String
    , roles : List Entity.Role
    }


pastExpiration : Time.Time -> String -> Bool
pastExpiration now token =
    case jwtDecoded token of
        Ok decoded ->
            decoded.exp < (Time.inSeconds now)

        -- are we past expiration?
        Err _ ->
            False


jwtDecoded : String -> Result Jwt.JwtError JwtPayload
jwtDecoded token =
    Jwt.decodeToken jwtPayloadDecoder token


jwtPayloadDecoder : JD.Decoder JwtPayload
jwtPayloadDecoder =
    JP.decode JwtPayload
        |> JP.required "aud" (JD.string)
        |> JP.required "exp" (JD.float)
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


createAuthRecord : String -> Entity.AuthRecord -> Http.Request Entity.Auth
createAuthRecord api authRecord =
    Http.request
        { method = "POST"
        , headers = []
        , url = api ++ "/auth"
        , body = (Entity.authRecordEncoder authRecord |> Http.jsonBody)
        , expect = Http.expectJson Entity.authDecoder
        , timeout = Nothing
        , withCredentials = False
        }


fetchGame : String -> String -> String -> Http.Request Entity.Game
fetchGame api token slug =
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


createUserRecord :
    String
    -> String
    -> Entity.UserRecord
    -> Http.Request Entity.User
createUserRecord api token user =
    Http.request
        { method = "POST"
        , headers = defaultHeaders token
        , url = api ++ "/user"
        , body = Http.jsonBody <| Entity.userRecordEncoder user
        , expect = Http.expectJson Entity.userDecoder
        , timeout = Nothing
        , withCredentials = False
        }


csvDataEncoder : CsvData -> JE.Value
csvDataEncoder v =
    JE.object <|
        List.filterMap identity <|
            [ (requiredFieldEncoder "upload" JE.string "" v.upload)
            , (requiredFieldEncoder "userid" JE.string "" v.userid)
            ]


csvDataDecoder : JD.Decoder CsvData
csvDataDecoder =
    JD.lazy <|
        \_ ->
            decode CsvData
                |> required "userid" JD.string ""
                |> required "upload" JD.string ""


uploadCsv :
    String
    -> String
    -> CsvData
    -> Http.Request CsvData
uploadCsv tasksrv token csvData =
    Http.request
        { method = "POST"
        , headers = defaultHeaders token
        , url = (tasksrv ++ "/upload/ugimgset")
        , body = Http.jsonBody <| csvDataEncoder csvData
        , expect = Http.expectJson csvDataDecoder
        , timeout = Nothing
        , withCredentials = False
        }


fetchGroup : String -> String -> String -> Http.Request Entity.Group
fetchGroup api token slug =
    Http.request
        { method = "GET"
        , headers = defaultHeaders token
        , url = api ++ "/group/" ++ slug
        , body = Http.emptyBody
        , expect = Http.expectJson Entity.groupDecoder
        , timeout = Nothing
        , withCredentials = False
        }


fetchRole : String -> String -> String -> Http.Request Entity.Role
fetchRole api token slug =
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


isAdmin : JwtPayload -> Bool
isAdmin jwt =
    List.map .name jwt.roles |> List.member "admin"
