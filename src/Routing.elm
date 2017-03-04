module Routing exposing (..)

import Navigation
import UrlParser exposing (..)
import Json.Decode as Decode
import Html.Events exposing (onWithOptions)
import Html exposing (Attribute)


type alias MenuItem =
    { name : String
    , route : Route
    , path : String
    }


initialMenuItems : List MenuItem
initialMenuItems =
    [ { name = "Home"
      , route = HomeRoute
      , path = homePath
      }
    , { name = "Badges"
      , route = BadgesRoute
      , path = badgesPath
      }
    , { name = "Instructions"
      , route = InstructionsRoute
      , path = instructionsPath
      }
    , { name = "Settings"
      , route = SettingsRoute
      , path = settingsPath
      }
    ]


{-|
Match a location given by the Navigation package and return the matched route.
-}
parseLocation : Navigation.Location -> Route
parseLocation location =
    case (UrlParser.parsePath matchers location) of
        Just route ->
            route

        Nothing ->
            NotFoundRoute


onLinkClick : msg -> Attribute msg
onLinkClick message =
    let
        options =
            { stopPropagation = False
            , preventDefault = True
            }
    in
        onWithOptions "click" options (Decode.succeed message)


{-| Represents where I am in the application
-}
type Route
    = AccessDeniedRoute
    | NotFoundRoute
    | LoginRoute
    | HomeRoute
      -- Visual Search
    | GameRouteVs
      -- Dot probe
    | GameRouteDp
      -- Go/No-go
    | GameRouteGn
      -- Stop Signale
    | GameRouteSs
      -- Respond Signal
    | GameRouteRs
    | BadgesRoute
    | SettingsRoute
    | InstructionsRoute


homePath : String
homePath =
    "/"


loginPath : String
loginPath =
    "/login"


logoutPath : String
logoutPath =
    "/logout"


badgesPath : String
badgesPath =
    "/badges"


settingsPath : String
settingsPath =
    "/settings"


instructionsPath : String
instructionsPath =
    "/instructions"



-- Private


{-|
Define how to match urls
-}
matchers : UrlParser.Parser (Route -> a) a
matchers =
    UrlParser.oneOf
        [ UrlParser.map HomeRoute UrlParser.top
        , UrlParser.map LoginRoute (UrlParser.s "login")
        , UrlParser.map GameRouteVs (UrlParser.s "vs")
        , UrlParser.map GameRouteDp (UrlParser.s "dp")
        , UrlParser.map GameRouteGn (UrlParser.s "gn")
        , UrlParser.map GameRouteSs (UrlParser.s "ss")
        , UrlParser.map BadgesRoute (UrlParser.s "badges")
        , UrlParser.map SettingsRoute (UrlParser.s "settings")
        , UrlParser.map InstructionsRoute (UrlParser.s "instructions")
        ]
