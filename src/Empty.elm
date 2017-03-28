module Empty exposing (..)

import Entity
import Model
import Routing
import Api
import Navigation


emptyModel : Model.Model -> Model.Model
emptyModel model =
    -- do not reset api or jwt
    { model
        | spin = False
        , activeRoute = Routing.LoginRoute
        , error = ""
        , presses = []
        , visitor = Model.Anonymous
        , menuIsActive = False
        , mainMenuItems = Routing.initMenuItems
        , currentTime = 0
        , currentTimeDelta = 0
        , user = emptyUser
        , authRecord = emptyAuthRecord
        , games = []
        , gimages = []
    }


emptyUser : Entity.User
emptyUser =
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
