module Todos exposing (..)

import Model
import Api
import Http


-- TODO, parse token, check roles, fetch data accordingly


initAdminStuff : String -> String -> List (Cmd Model.Msg)
initAdminStuff api token =
    -- Game stuff
    [ Http.send Model.UsersResp (Api.fetchUsers api token)
    , Http.send Model.GroupResp (Api.fetchGroup api token "control_a")
    , Http.send Model.GroupResp (Api.fetchGroup api token "experimental_a")
    , Http.send Model.RoleResp (Api.fetchRole api token "user")
    ]


initUserStuff : String -> String -> List (Cmd Model.Msg)
initUserStuff api token =
    [ Http.send Model.GameResp (Api.fetchGame api token "gonogo")
    , Http.send Model.GameResp (Api.fetchGame api token "dotprobe")
    , Http.send Model.GameResp (Api.fetchGame api token "stopsignal")
    , Http.send Model.GameResp (Api.fetchGame api token "respondsignal")
    , Http.send Model.GameResp (Api.fetchGame api token "visualsearch")
    ]
