module Empty exposing (..)

import Entity
import Model
import Routing
import Auth
import Navigation


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


emptyModel : Model.Model
emptyModel =
    { authRecord = emptyAuthRecord
    , spin = False
    , activeRoute = Routing.LoginRoute
    , changes = 0
    , api = "http://localhost:8680"
    , jwtencoded = ""
    , jwtdecoded = Auth.jwtDecoded ""
    , error = ""
    , presses = []
    , user = emptyUser
    , menuIsActive = False
    , mainMenuItems = Routing.initMenuItems
    , greeting = ""
    , test = ""
    , currentTime = 0
    , currentTimeDelta = 0
    , games = []
    , gimages = []
    }
