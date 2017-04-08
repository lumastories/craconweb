module Todos exposing (..)

import Model
import Api
import Http


--import Port

import Navigation
import Routing


--import Entity
-- TODO, parse token, check roles, fetch data accordingly


initCommands : String -> String -> List (Cmd Model.Msg)
initCommands httpsrv token =
    case Api.jwtDecoded token of
        Ok jwt ->
            case Api.isAdmin jwt of
                True ->
                    [ Http.send Model.UsersResp (Api.fetchUsers httpsrv token)
                    , Http.send Model.RoleResp (Api.fetchRole httpsrv token "user")
                    , Http.send Model.GroupResp (Api.fetchGroup httpsrv token "control_a")
                    , Http.send Model.GroupResp (Api.fetchGroup httpsrv token "experimental_a")
                    , Http.send Model.GameResp (Api.fetchGame httpsrv token "gonogo")
                    , Http.send Model.GameResp (Api.fetchGame httpsrv token "dotprobe")
                    , Http.send Model.GameResp (Api.fetchGame httpsrv token "stopsignal")
                    , Http.send Model.GameResp (Api.fetchGame httpsrv token "respondsignal")
                    , Http.send Model.GameResp (Api.fetchGame httpsrv token "visualsearch")
                    , Navigation.newUrl Routing.adminPath
                      -- , Port.set ( "user", Entity.userEncoder user )
                    ]

                False ->
                    [ Http.send Model.GameResp (Api.fetchGame httpsrv token "gonogo")
                    , Http.send Model.GameResp (Api.fetchGame httpsrv token "dotprobe")
                    , Http.send Model.GameResp (Api.fetchGame httpsrv token "stopsignal")
                    , Http.send Model.GameResp (Api.fetchGame httpsrv token "respondsignal")
                    , Http.send Model.GameResp (Api.fetchGame httpsrv token "visualsearch")
                    , Navigation.newUrl "/"
                      -- , Port.set ( "user", Entity.userEncoder user )
                    ]

        Err _ ->
            []
