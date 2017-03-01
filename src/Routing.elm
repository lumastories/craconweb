module Routing exposing (..)

import Navigation
import UrlParser exposing (..)
import Json.Decode as Decode
import Html.Events exposing (onWithOptions)
import Html exposing (Attribute)


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
    | GameRoute String
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
        , UrlParser.map GameRoute (UrlParser.s "game" </> UrlParser.string)
        , UrlParser.map BadgesRoute (UrlParser.s "badges")
        , UrlParser.map SettingsRoute (UrlParser.s "settings")
        , UrlParser.map InstructionsRoute (UrlParser.s "instructions")
        ]
