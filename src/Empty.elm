module Empty exposing (..)

import Api
import Entity
import Model
import Navigation
import Routing


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


emptyAdminModel : Model.AdminModel
emptyAdminModel =
    { users =
        []
        -- TODO user registration data modelling
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
