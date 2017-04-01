module Empty exposing (..)

import Api
import Entity
import Model
import Navigation
import Routing
import Dict


emptyModel : Model.Model -> Model.Model
emptyModel model =
    { model
        | loading = ( False, "" )
        , activeRoute = Routing.LoginRoute
        , presses = []
        , jwtencoded = ""
        , visitor = Model.Anonymous
        , isMenuActive = False
        , mainMenuItems = Routing.initMenuItems
        , currentTime = 0
        , currentTimeDelta = 0
        , user = emptyUser
        , authRecord = emptyAuthRecord
        , glitching = ( False, "" )
        , responseTimes = []
        , startTime = 0
    }


emptyUser : Entity.User
emptyUser =
    { id = ""
    , username = ""
    , email = ""
    , firstName = ""
    , lastName = ""
    , avatar = ""
    , groupID = ""
    , roles = []
    , lastLogin = Nothing
    , blocked = Nothing
    , created = Nothing
    , updated = Nothing
    , deleted = Nothing
    }


emptyUserRecord =
    { username = ""
    , email = ""
    , firstName = ""
    , lastName = ""
    , avatar = ""
    , groupID = ""
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


emptyUserRole =
    { id = ""
    , name = "user"
    , weight = 0
    }


emptyGame =
    { id = ""
    , name = ""
    , slug = ""
    , dscript = ""
    , icon = ""
    , reactDur = 0
    , sessDur = 0
    , trialDur = 0
    , offsetDur = 0
    , fixDur = 0
    , fixImg = ""
    , durInc = 0
    , durDec = 0
    , incTrigger = 0
    , decTrigger = 0
    , blocked = Nothing
    , created = Nothing
    , updated = Nothing
    , deleted = Nothing
    }
