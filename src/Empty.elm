module Empty exposing (..)

import Entity
import Model
import Navigation
import Routing
import Game


emptyModel : Model.Model -> Model.Model
emptyModel model =
    { model
        | activeRoute = Routing.LoginRoute
        , jwtencoded = ""
        , visitor = Model.Anon
        , users = []
        , isMenuActive = False
        , user = Nothing
        , login = { username = "", password = "" }
        , loading = Nothing
        , glitching = Nothing
        , gameState = Game.NotPlaying
        , fmriUserData = Nothing
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
