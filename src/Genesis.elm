module Genesis exposing (..)

import Entity


initUser : Entity.User
initUser =
    { id = 0
    , username = ""
    , email = ""
    , firstName = ""
    , lastName = ""
    , avatar = ""
    , groupID = 0
    , roles = []
    , lastLogin = Nothing
    , blocked = Nothing
    , created = Nothing
    , updated = Nothing
    , deleted = Nothing
    }


initAuthRecord : Entity.AuthRecord
initAuthRecord =
    { email = ""
    , password = ""
    }
