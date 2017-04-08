module Api exposing (jwtDecoded, okyToky, isAdmin, fetchGame, fetchUser, fetchUsers, fetchGroup, fetchRole, createUserRecord, createAuthRecord, fetchAll)

import Entity
import Http
import Jwt
import Time
import Model as M
import Json.Decode as JD


fetchAll : String -> M.JwtPayload -> String -> Cmd M.Msg
fetchAll httpsrv jwt token =
    case isAdmin jwt of
        True ->
            Cmd.batch <|
                (userData httpsrv token jwt.sub)
                    ++ (adminData httpsrv token)

        False ->
            Cmd.batch <| userData httpsrv token jwt.sub


adminData : String -> String -> List (Cmd M.Msg)
adminData httpsrv token =
    [ Http.send M.UsersResp (fetchUsers httpsrv token)
    , Http.send M.RoleResp (fetchRole httpsrv token "user")
    , Http.send M.GroupResp (fetchGroup httpsrv token "control_a")
    , Http.send M.GroupResp (fetchGroup httpsrv token "experimental_a")
    ]


userData : String -> String -> String -> List (Cmd M.Msg)
userData httpsrv token sub =
    [ Http.send M.GameResp (fetchGame httpsrv token "gonogo")
    , Http.send M.GameResp (fetchGame httpsrv token "dotprobe")
    , Http.send M.GameResp (fetchGame httpsrv token "stopsignal")
    , Http.send M.GameResp (fetchGame httpsrv token "respondsignal")
    , Http.send M.GameResp (fetchGame httpsrv token "visualsearch")
    , Http.send M.UserResp (fetchUser httpsrv token sub)
    ]


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


fetchUgimages : String -> String -> String -> String -> String -> Http.Request Entity.Ugimages
fetchUgimages httpsrv token ugimgsetId limit gimgtypeSlug =
    getRequest token
        (httpsrv
            ++ "/ugimgset/"
            ++ ugimgsetId
            ++ "/ugimages?valDesc=true&limit="
            ++ limit
            ++ "&valEach=true&gimgtypeSlug="
            ++ gimgtypeSlug
        )
        Entity.ugimagesDecoder


fetchUgimgsets : String -> String -> String -> Http.Request Entity.Ugimgsets
fetchUgimgsets httpsrv token sub =
    getRequest token
        (httpsrv
            ++ "/user/"
            ++ sub
            ++ "/ugimgsets?createdDesc=true&limit=1&createdEach=true"
        )
        Entity.ugimgsetsDecoder


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


isAdmin : M.JwtPayload -> Bool
isAdmin jwt =
    List.map .name jwt.roles |> List.member "admin"


okyToky : Time.Time -> String -> Result String M.JwtPayload
okyToky now token =
    case Jwt.decodeToken M.jwtDecoder token of
        Ok decoded ->
            case Jwt.isExpired now token of
                Ok _ ->
                    Ok decoded

                _ ->
                    Err "Expired"

        _ ->
            Err "Decoding problem"


jwtDecoded : String -> Result Jwt.JwtError M.JwtPayload
jwtDecoded token =
    Jwt.decodeToken M.jwtDecoder token
