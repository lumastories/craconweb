module Api exposing (..)

import Entity
import Http
import Json.Decode as JD
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


emptyJwtPayload : JwtPayload
emptyJwtPayload =
    { aud = ""
    , exp = 0
    , iat = 0
    , iss = ""
    , sub = ""
    , roles = []
    }



--type Result error value
--    = Ok value
--    | Err error


okyToky : Time.Time -> String -> Result String JwtPayload
okyToky now token =
    case Jwt.decodeToken jwtDecoder token of
        Ok decoded ->
            case Jwt.isExpired now token of 
                Ok _ ->
                    Ok decoded
                _ ->
                    Err "Expired"
        _ ->
            Err "Decoding problem"


-- TODO remove this
jwtDecoded : String -> Result Jwt.JwtError JwtPayload
jwtDecoded token =
    Jwt.decodeToken jwtDecoder token


jwtDecoder : JD.Decoder JwtPayload
jwtDecoder =
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
createAuthRecord httpsrv authRecord =
    Http.request
        { method = "POST"
        , headers = []
        , url = httpsrv ++ "/auth"
        , body = (Entity.authRecordEncoder authRecord |> Http.jsonBody)
        , expect = Http.expectJson Entity.authDecoder
        , timeout = Nothing
        , withCredentials = False
        }


fetchGame : String -> String -> String -> Http.Request Entity.Game
fetchGame httpsrv token slug =
    getRequest token (httpsrv ++ "/game/" ++ slug) Entity.gameDecoder


fetchUser : String -> String -> String -> Http.Request Entity.User
fetchUser httpsrv token sub =
    getRequest token (httpsrv ++ "/user/" ++ sub) Entity.userDecoder


fetchUsers : String -> String -> Http.Request (List Entity.User)
fetchUsers httpsrv token =
    getRequest token (httpsrv ++ "/users?createdEach=true") (JD.field "users" (JD.list Entity.userDecoder))


fetchGroup : String -> String -> String -> Http.Request Entity.Group
fetchGroup httpsrv token slug =
    getRequest token (httpsrv ++ "/group/" ++ slug) Entity.groupDecoder


fetchRole : String -> String -> String -> Http.Request Entity.Role
fetchRole httpsrv token slug =
    getRequest token (httpsrv ++ "/role/" ++ slug) Entity.roleDecoder


createUserRecord :
    String
    -> String
    -> Entity.UserRecord
    -> Http.Request Entity.User
createUserRecord httpsrv token user =
    Http.request
        { method = "POST"
        , headers = defaultHeaders token
        , url = httpsrv ++ "/user"
        , body = Http.jsonBody <| Entity.userRecordEncoder user
        , expect = Http.expectJson Entity.userDecoder
        , timeout = Nothing
        , withCredentials = False
        }


csvDataDecoder : JD.Decoder CsvData
csvDataDecoder =
    JD.lazy <|
        \_ ->
            decode CsvData
                |> required "userid" JD.string ""
                |> required "upload" JD.string ""


csvDataParts : CsvData -> List Http.Part
csvDataParts data =
    [ Http.stringPart "upload" data.upload
    , Http.stringPart "userid" data.userid
    ]


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
        , body = Http.multipartBody <| csvDataParts csvData
        , expect = Http.expectJson csvDataDecoder
        , timeout = Nothing
        , withCredentials = False
        }


getRequest : String -> String -> JD.Decoder a -> Http.Request a
getRequest token endpoint jsonDecoder =
    Http.request
        { method = "GET"
        , headers = defaultHeaders token
        , url = endpoint
        , body = Http.emptyBody
        , expect = Http.expectJson jsonDecoder
        , timeout = Nothing
        , withCredentials = False
        }


type alias ErrorCode =
    { error : String
    , code : Int
    }


decodeErrorCode : String -> ErrorCode
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
