module Router exposing (delta2url, location2messages)

import Model exposing (..)
import Navigation exposing (Location)
import RouteUrl exposing (HistoryEntry(..), UrlChange)


delta2url : Model -> Model -> Maybe UrlChange
delta2url previous current =
    case current.activePage of
        AccessDenied ->
            Nothing

        PageNotFound ->
            Just <| UrlChange NewEntry "/#404"

        Login ->
            Just <| UrlChange NewEntry "/#login"

        Games ->
            Just <| UrlChange NewEntry "/#games"

        Badges ->
            Just <| UrlChange NewEntry "/#badges"


location2messages : Location -> List Msg
location2messages location =
    case location.hash of
        "" ->
            []

        "#login" ->
            [ SetActivePage Login ]

        "#games" ->
            [ SetActivePage Games ]

        "#badges" ->
            [ SetActivePage Badges ]

        "#404" ->
            [ SetActivePage PageNotFound ]

        _ ->
            [ SetActivePage PageNotFound ]
