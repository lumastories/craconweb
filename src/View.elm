module View exposing (view)

import Html exposing (..)
import Model exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Routing as R


-- TODO Create a new logo (brain with CC - craving control, crave control)
-- TODO Select a color pallette (check color blind compatability)
-- TODO Create reset password modal
-- TODO Write decoders and encoders for swagger
-- TODO Auto generate models, encoders and decoders from swagger (whaaaa) or use IDL or GRPC -> elm


view : Model -> Html Msg
view model =
    let
        page =
            case model.activeRoute of
                R.LoginRoute ->
                    loginPage model

                R.HomeRoute ->
                    homePage model

                R.AccessDeniedRoute ->
                    accessDeniedPage model

                R.NotFoundRoute ->
                    notFoundPage model

                R.BadgesRoute ->
                    badgesPage model

                R.SettingsRoute ->
                    settingsPage model

                R.InstructionsRoute ->
                    instructionsPage model

                R.GameRouteVs ->
                    gamePage model

                R.GameRouteDp ->
                    gamePage model

                R.GameRouteGn ->
                    gamePage model

                R.GameRouteSs ->
                    gamePage model

                R.GameRouteRs ->
                    gamePage model
    in
        div [] [ page ]



-- LOGIN PAGE


loginPage : Model -> Html Msg
loginPage model =
    section []
        [ div [ class "container" ]
            [ div [ class "columns is-desktop" ]
                [ div [ class "column is-half is-offset-one-quarter" ]
                    [ logo loginPageLogoWidth
                    , loginPageBoxForm model
                    , p [ class "has-text-centered" ]
                        [ em [] [ text model.error ]
                        ]
                    ]
                ]
            ]
        ]



-- TODO add reset password functionality
-- TODO remove css.map, don't need it.


loginPageLogoWidth : Int
loginPageLogoWidth =
    200


loginPageButtonClass : Bool -> String
loginPageButtonClass spin =
    case spin of
        False ->
            "button is-dark"

        True ->
            "button is-dark is-loading"


loginPageBoxForm : Model -> Html Msg
loginPageBoxForm model =
    div [ class "box", marginS ]
        [ div
            []
            [ label [ class "label" ]
                [ text "Email" ]
            , p [ class "control" ]
                [ input [ class "input", placeholder "Email", type_ "email", onInput UpdateEmail ]
                    []
                ]
            , label [ class "label" ]
                [ text "Password" ]
            , p [ class "control" ]
                [ input [ class "input", placeholder "Password", type_ "password", onInput UpdatePassword ]
                    []
                ]
            , hr []
                []
            , p [ class "control" ]
                [ button [ class <| loginPageButtonClass model.spin, onClick TryLogin ] [ text "Let's Go!" ]
                ]
            ]
        ]



-- NAV BAR


aattrs linkPath =
    [ href linkPath, R.onLinkClick <| UpdateLocation linkPath ]


navLink text_ activeRoute ( route, linkPath ) =
    let
        class_ =
            if route == activeRoute then
                "nav-item is-tab is-hidden-mobile is-active"
            else
                "nav-item is-tab is-hidden-mobile"

        onClick_ =
            R.onLinkClick <| UpdateLocation linkPath
    in
        a
            [ class class_, href linkPath, onClick_ ]
            [ text text_ ]


navToggle =
    span
        [ class "nav-toggle" ]
        [ span [] []
        , span [] []
        , span [] []
        ]


navBarMenuLeftItems model =
    let
        logo =
            a [ class "nav-item", href <| R.homePath, R.onLinkClick <| UpdateLocation R.homePath ] [ img [ src "img/logo.png" ] [] ]

        toItem i =
            navLink i.name model.activeRoute ( i.route, i.path )
    in
        logo :: List.map toItem model.mainMenuItems


navBar model =
    nav
        [ class "nav has-shadow" ]
        [ div
            [ class "container" ]
            [ div
                [ class "nav-left" ]
                (navBarMenuLeftItems model)
            , span
                [ class "nav-toggle", onClick MainMenuToggle ]
                [ span [] [], span [] [], span [] [] ]
            , navRight model
            ]
        ]


navRight model =
    div
        [ class "nav-right nav-menu" ]
        [ a
            [ class "nav-item is-tab", onClick Logout ]
            [ text "Log out"
            ]
        ]



-- HOME PAGE


homePage : Model -> Html Msg
homePage model =
    div []
        [ navBar model
        , homePageBody model
        ]


homePageBody : Model -> Html Msg
homePageBody model =
    section
        [ class "hero is-primary" ]
        [ div
            [ class "hero-body" ]
            [ div
                [ class "container" ]
                [ homePageGrid model
                ]
            ]
        ]


homePageGameCards model =
    let
        toCard g =
            div [ class "column" ] [ homePageGameCard g.slug g.icon g.name g.about ]
    in
        List.map toCard model.games


homePageGrid : Model -> Html Msg
homePageGrid model =
    div [ class "columns" ]
        (homePageGameCards model)



-- TODO Change all links to function calls, store loginPath in variable in routing. R.loginPath.
-- TODO No more hacky strings! THis allows us to change the naming conventions whenever we want
-- TODO utilize bulma more!
-- Tags: http://bulma.io/documentation/elements/tag/
-- Progress: http://bulma.io/documentation/elements/progress/
-- Notification:
-- links!
-- a [ href loginPath, onLinkClick <| UpdateLocation loginPath ] [ text "Go to login" ]


homePageGameCard : String -> String -> String -> String -> Html Msg
homePageGameCard gameSlug src_ title about =
    div
        [ class "card", style <| toStyle "border-radius:1em;" ]
        [ div
            [ class "card-image" ]
            [ figure
                [ class "image is-4by3" ]
                [ a [ href <| gameSlug, R.onLinkClick <| UpdateLocation <| gameSlug ]
                    [ img
                        [ src src_, alt title ]
                        []
                    ]
                ]
            ]
        , div
            [ class "card-content" ]
            [ div
                [ class "media" ]
                [ div
                    [ class "media-content" ]
                    [ strong
                        []
                        [ text title
                        ]
                    ]
                ]
            , div
                [ class "content" ]
                [ text about
                , small
                    []
                    [ text "..."
                    ]
                ]
            ]
        ]



-- The parent of basic pages.


basicPage model children =
    section []
        [ navBar model
        , section
            [ class "section" ]
            children
        ]


accessDeniedPage : Model -> Html Msg
accessDeniedPage model =
    basicPage model
        [ div
            [ class "container" ]
            [ h1 [ class "title is-1" ] [ text "Access Restricted." ]
            , h3 [] [ text "Maybe you are in the wrong place?" ]
            ]
        ]


notFoundPage : Model -> Html Msg
notFoundPage model =
    basicPage model
        [ div
            [ class "container" ]
            [ h1 [ class "title is-1" ] [ text "Page Not Found (Oops!)" ]
            , h3 [] [ text "Maybe you are in the wrong place?" ]
            ]
        ]


badgesPage : Model -> Html Msg
badgesPage model =
    basicPage model
        [ div
            [ class "container" ]
            [ h1 [ class "title is-1" ] [ text "Badges" ]
            , h3 [] [ text "This feature is coming soon!" ]
            , i [ class "fa fa-certificate" ] []
            ]
        ]



-- TODO on badgesPage, use a Modal Card
-- http://bulma.io/documentation/components/modal/


settingsPage : Model -> Html Msg
settingsPage model =
    basicPage model
        [ div
            [ class "container" ]
            [ h1 [ class "title is-1" ] [ text "Settings" ]
            , h3 [] [ text "This feature is coming soon!" ]
            ]
        ]


gamePage : Model -> Html Msg
gamePage model =
    basicPage model
        [ div
            [ class "container" ]
            [ h1 [ class "title is-1" ] [ text "A Game is coming soon" ]
            ]
        ]


instructionsPage model =
    basicPage model
        [ div
            [ class "container" ]
            [ h1 [ class "title is-1" ] [ text "Instructions" ]
            ]
        ]



-- TODO fetch data for game, indicate loading progress
-- TODO display stimuli and record user responses
-- TODO track partial data, attempt to save
{--TODO map over model.mainMenuItems and return
[ mainMenuLink "Home" R.homePath (activeRoute_ == R.HomeRoute)
, mainMenuLink "Badges" R.badgesPath (activeRoute_ == R.BadgesRoute)
, mainMenuLink "Instructions" R.instructionsPath (activeRoute_ == R.InstructionsRoute)
, mainMenuLink "Settings" R.settingsPath (activeRoute_ == R.SettingsRoute) ]
--}
-- HELPERS


marginS : Attribute msg
marginS =
    style [ ( "margin", ".7em" ) ]


style_ : String -> Attribute msg
style_ rawStyles =
    style (toStyle rawStyles)


toStyle : String -> List ( String, String )
toStyle styles =
    -- "a:b;c:d;" -> ["a:b", "c:d", ""]
    -- ["a:b", "c:d"] -> [["a", "b"], ["c","d"], [""]]
    -- [["a", "b"], ["c","d"]]
    -- [["a", "b"], ["c","d"]] -> [("a", "b"), ("c","d")]
    String.split ";" styles
        |> List.map (\s -> String.split ":" s)
        |> List.filter (\e -> e /= [ "" ])
        |> List.map toStylesHelper


toStylesHelper : List String -> ( String, String )
toStylesHelper list =
    case list of
        [ a, b ] ->
            ( a, b )

        _ ->
            ( "", "" )


sizePx : Int -> String
sizePx size =
    (toString size) ++ "px"


centerStyled : Attribute msg
centerStyled =
    style [ ( "text-align", "center" ) ]


logo : Int -> Html Msg
logo size =
    p [ centerStyled ] [ img [ class "logo is-vcentered", src "img/logo.svg", style [ ( "max-width", sizePx size ) ] ] [] ]
