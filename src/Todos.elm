module Todos exposing (..)

import Model
import Api
import Http
import Port


-- TODO, parse token, check roles, fetch data accordingly


initCommands : String -> String -> List (Cmd Model.Msg)
initCommands api token =
    case Api.jwtDecoded token of
        Ok jwt ->
            case Api.isAdmin jwt of
                True ->
                    [ Http.send Model.UsersResp (Api.fetchUsers api token)
                    , Http.send Model.RoleResp (Api.fetchRole api token "user")
                    , Http.send Model.GroupResp (Api.fetchGroup api token "control_a")
                    , Http.send Model.GroupResp (Api.fetchGroup api token "experimental_a")
                    , Http.send Model.GameResp (Api.fetchGame api token "gonogo")
                    , Http.send Model.GameResp (Api.fetchGame api token "dotprobe")
                    , Http.send Model.GameResp (Api.fetchGame api token "stopsignal")
                    , Http.send Model.GameResp (Api.fetchGame api token "respondsignal")
                    , Http.send Model.GameResp (Api.fetchGame api token "visualsearch")
                    , Port.storageGetItem "user"
                    , Navigation.newUrl Routing.adminPath
                    ]

                False ->
                    [ Http.send Model.GameResp (Api.fetchGame api token "gonogo")
                    , Http.send Model.GameResp (Api.fetchGame api token "dotprobe")
                    , Http.send Model.GameResp (Api.fetchGame api token "stopsignal")
                    , Http.send Model.GameResp (Api.fetchGame api token "respondsignal")
                    , Http.send Model.GameResp (Api.fetchGame api token "visualsearch")
                    , Port.storageGetItem "user"
                    , Navigation.newUrl "/"
                    ]

        Err _ ->
            []
