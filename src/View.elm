module View exposing (view)

import Html exposing (..)
import Model exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Routing as R
import Entity


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
                    visualSearchGame model

                R.GameRouteDp ->
                    dotProbeGame model

                R.GameRouteGn ->
                    goNoGoGame model

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
                        [ em [] [ text model.greeting ]
                        , em [] [ text model.error ]
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



-- onWithOptions
-- onWithOptions
--"click"
--{ preventDefault = True, stopPropagation = True }
--(Json.succeed msg)


loginPageBoxForm : Model -> Html Msg
loginPageBoxForm model =
    div [ class "box", marginS ]
        [ Html.form
            [ onSubmit TryLogin ]
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


aattrs : String -> List (Attribute Msg)
aattrs linkPath =
    [ href linkPath, R.onLinkClick <| UpdateLocation linkPath ]


navLink : String -> R.Route -> ( R.Route, String ) -> String -> Html Msg
navLink text_ activeRoute ( route, linkPath ) hidden =
    let
        class_ =
            if route == activeRoute then
                "nav-item is-tab is-active " ++ hidden
            else
                "nav-item is-tab " ++ hidden

        onClick_ =
            R.onLinkClick <| UpdateLocation linkPath
    in
        a
            [ class class_, href linkPath, onClick_ ]
            [ text text_ ]


navToggle : Html Msg
navToggle =
    span
        [ class "nav-toggle" ]
        [ span [] []
        , span [] []
        , span [] []
        ]


navBarMenuLeftItems : Model -> List (Html Msg)
navBarMenuLeftItems model =
    let
        logo =
            a [ class "nav-item", href <| R.homePath, R.onLinkClick <| UpdateLocation R.homePath ] [ img [ src "img/logo.png" ] [] ]

        toItem i =
            navLink i.name model.activeRoute ( i.route, i.path ) "is-hidden-mobile"
    in
        logo :: List.map toItem model.mainMenuItems


navBarMenuMobileItems : Model -> List (Html Msg)
navBarMenuMobileItems model =
    let
        toItem i =
            navLink i.name model.activeRoute ( i.route, i.path ) "is-hidden-tablet"
    in
        List.map toItem model.mainMenuItems


navBar : Model -> Html Msg
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


navRight : Model -> Html Msg
navRight model =
    let
        logout =
            a [ class "nav-item is-tab", onClick Logout ] [ text "Log out" ]
    in
        div
            [ class "nav-right nav-menu" ]
            (logout :: (navBarMenuMobileItems model))



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


homePageGrid : Model -> Html Msg
homePageGrid model =
    div [ class "columns" ]
        (homePageGameCards model)


homePageGameCards : Model -> List (Html Msg)
homePageGameCards model =
    let
        toCard g =
            div [ class "column" ] [ homePageGameCard g.slug g.icon g.name g.dscript ]
    in
        List.map toCard model.games



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
                        [ src <| "http://localhost:8654/repo/" ++ src_, alt title ]
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


basicPage : Model -> List (Html Msg) -> Html Msg
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


badge : String -> Html Msg
badge text_ =
    p [] [ i [ class "fa fa-certificate fa-5x" ] [], text text_ ]


badgesPage : Model -> Html Msg
badgesPage model =
    basicPage model
        [ div
            [ class "container" ]
            [ h1 [ class "title is-1" ] [ text "Badges" ]
            , h3 [] [ text "Select a badge below to see how to unlock it" ]
            , badge "1"
            , badge "2"
            , badge "3"
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


visualSearchGame model =
    basicPage model
        [ div
            [ class "container" ]
            [ h1 [ class "title is-1" ] [ text "Visual Search" ]
            , div [] [ text "game goes here" ]
            ]
        ]




gameName : Model -> String -> Maybe String
gameName model slug_ =
        List.filter (\g -> g.slug == slug_) model.games
        |> List.map .name
        |> List.head

goNoGoGame model =
    let
        name = 
                Maybe.withDefault "" (gameName model "gonogo")
    in
        basicPage model
                [ div
                [ class "container" ]
                [ h1 [ class "title is-1" ] [ text name ]
                ]
                ]



-- TODO try this pattern out some where else


setTime : Msg
setTime =
    GetTimeAndThen (\time -> CalcTimeDelta time)


dotProbeGame : Model -> Html Msg
dotProbeGame model =
    basicPage model
        [ div
            [ class "container" ]
            [ h1 [ class "title is-1" ] [ text "Dot Probe" ]
            , h3 [ class "subtitle is-3" ] [ text "work in progress" ]
            ]
        ]


liImg : String -> Html Msg
liImg src_ =
    li [] [ img [ src src_ ] [] ]


listOfStims : List Entity.Gimage -> List (Html Msg)
listOfStims gimages =
    gimages
        |> List.map .path
        |> List.map liImg


instructionsPage : Model -> Html Msg
instructionsPage model =
    basicPage model
        [ div
            [ class "container" ]
            [ h1 [ class "title is-1" ] [ text "Instructions" ]
            ]
        ]



-- TODO make things more gamey with animations!
-- https://github.com/mdgriffith/elm-style-animation/blob/master/examples/Showcase.elm


marginS : Attribute msg
marginS =
    style [ ( "margin", ".7em" ) ]


style_ : String -> Attribute msg
style_ rawStyles =
    style (toStyle rawStyles)


toStyle : String -> List ( String, String )
toStyle styles =
    -- "a:b;c:d;" |>|>|> [("a", "b"), ("c","d")]
    let
        toTuple list =
            case list of
                [ a, b ] ->
                    ( a, b )

                _ ->
                    ( "", "" )
    in
        String.split ";" styles
            |> List.map (\s -> String.split ":" s)
            |> List.filter (\e -> e /= [ "" ])
            |> List.map toTuple


sizePx : Int -> String
sizePx size =
    (toString size) ++ "px"


centerStyled : Attribute msg
centerStyled =
    style [ ( "text-align", "center" ) ]


logo : Int -> Html Msg
logo size =
    p [ centerStyled ] [ img [ class "logo is-vcentered", src "img/logo.svg", style [ ( "max-width", sizePx size ) ] ] [] ]
