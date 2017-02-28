module Routing exposing (onLinkClick, parseLocation, homePath, loginPath, Route(..))

import Navigation
import UrlParser
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
    | BadgesRoute


homePath : String
homePath =
    "/"


loginPath : String
loginPath =
    "/login"



-- Private


{-|
Define how to match urls
-}
matchers : UrlParser.Parser (Route -> a) a
matchers =
    UrlParser.oneOf
        [ UrlParser.map HomeRoute UrlParser.top
        , UrlParser.map LoginRoute (UrlParser.s "login")
          -- TODO add to this, also use #
        ]
