module Routing exposing (..)

import Html exposing (Attribute)
import Html.Events exposing (onWithOptions)
import Json.Decode as Decode
import Navigation
import UrlParser exposing (..)


adminPaths : List String
adminPaths =
    [ adminPath, registerPath ]


type alias MenuItem =
    { name : String
    , route : Route
    , path : String
    }


{-|
Match a location given by the Navigation package and return the matched route.
-}
parseLocation : Navigation.Location -> Route
parseLocation location =
    case (parsePath matchers location) of
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
      -- | GameRouteRs
    | BadgesRoute
    | SettingsRoute
    | InstructionsRoute
    | AdminRoute
    | RegisterRoute
    | EditUserRoute String


homePath : String
homePath =
    "/"


loginPath : String
loginPath =
    "/login"


adminPath : String
adminPath =
    "/admin"


registerPath : String
registerPath =
    "/register"


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


editPath : String
editPath =
    "/edit/"



-- Private


{-|
Define how to match urls
-}
matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map HomeRoute top
        , map LoginRoute (s "login")
        , map GameRouteVs (s "visualsearch")
        , map GameRouteDp (s "dotprobe")
        , map GameRouteGn (s "gonogo")
        , map GameRouteSs (s "stopsignal")
          -- , map GameRouteRs (s "respondsignal")
        , map BadgesRoute (s "badges")
        , map SettingsRoute (s "settings")
        , map InstructionsRoute (s "instructions")
        , map AdminRoute (s "admin")
        , map RegisterRoute (s "register")
        , map EditUserRoute (s "edit" </> string)
        ]
