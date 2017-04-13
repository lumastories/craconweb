module Empty exposing (..)

import Entity
import Model
import Navigation
import Routing


emptyModel : Model.Model -> Model.Model
emptyModel model =
    { model
        | activeRoute = Routing.LoginRoute
        , presses = []
        , jwtencoded = ""
        , visitor = Model.Anon
        , users = []
        , isMenuActive = False
        , user = Nothing
        , authRecord = emptyAuthRecord
        , loading = Nothing
        , glitching = Nothing
        , playingGame = Nothing
    }


emptyUserRecord : Entity.UserRecord
emptyUserRecord =
    { username = ""
    , email = ""
    , firstName = ""
    , lastName = ""
    , avatar = ""
    , groupId = ""
    , roles = []
    , password = ""
    }


emptyAuthRecord : Entity.AuthRecord
emptyAuthRecord =
    { email = ""
    , password = ""
    }


emptyLocation : Navigation.Location
emptyLocation =
    { href = ""
    , host = ""
    , hostname = ""
    , protocol = ""
    , origin = ""
    , port_ = ""
    , pathname = ""
    , search = ""
    , hash = ""
    , username = ""
    , password = ""
    }


emptyRole : Entity.Role
emptyRole =
    { id = ""
    , name = "user"
    , weight = 0
    }
