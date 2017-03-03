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

                R.GameRoute gameSlug ->
                    gamePicker model gameSlug

                _ ->
                    p []
                        [ i [ class "fa fa-certificate" ] []
                        , text "route not implemented yet"
                        ]
    in
        div [] [ page ]


gamePicker : Model -> String -> Html Msg
gamePicker model gameSlug =
    case gameSlug of
        "vs" ->
            gamePage model "vs"

        "dp" ->
            gamePage model "dp"

        "gn" ->
            gamePage model "gn"

        "ss" ->
            gamePage model "ss"

        "rs" ->
            gamePage model "rs"

        _ ->
            gamePage model "could not find this game, sorry!"



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
        [ Html.form
            [ onSubmit TryLogin
            , action "javascript:void(0);"
            ]
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



-- HOME PAGE


homePage : Model -> Html Msg
homePage model =
    section []
        [ homePageHeader model
        , homePageBody model
        ]


homePageHeader : Model -> Html Msg
homePageHeader model =
    section
        [ class "hero" ]
        [ div
            [ class "hero-body" ]
            [ div
                [ class "container" ]
                [ logoSmall
                , h1
                    [ class "title" ]
                    [ text <| "Welcome, " ++ model.user.firstName ]
                , h2
                    [ class "subtitle" ]
                    [ text "Nice to see you" ]
                , mainMenuButton
                , mainMenu model.activeRoute model.menuIsActive
                ]
            ]
        ]


homePageBody : Model -> Html Msg
homePageBody model =
    section
        [ class "section" ]
        [ div
            [ class "container" ]
            [ homePageGrid model ]
        ]


homePageGrid : Model -> Html Msg
homePageGrid model =
    div [ class "columns" ]
        [ div [ class "column" ] [ homePageGameCard "vs" "img/icons1.png" "Visual Search" "Find the right thing." "This game is very interesting. Here is even more info about the game" ]
        , div [ class "column" ] [ homePageGameCard "dp" "img/icons2.png" "Dot Probe" "Watch the dot." "This game is very interesting. Here is even more info about the game" ]
        , div [ class "column" ] [ homePageGameCard "gn" "img/icons3.png" "Go/No-Go" "Don't hesitate." "This game is very interesting. Here is even more info about the game" ]
        , div [ class "column" ] [ homePageGameCard "ss" "img/icons4.png" "Stop Signal" "Watch for the signal!" "This game is very interesting. Here is even more info about the game" ]
        , div [ class "column" ] [ homePageGameCard "rs" "img/icons5.png" "Respond Signal" "Respond. Oh the signalling!" "This game is very interesting. Here is even more info about the game" ]
          --[ text "Respond Signal"
          --, a [ href loginPath, onLinkClick <| UpdateLocation loginPath ] [ text "Go to login" ]
          --]
        ]



-- TODO Change all links to function calls, store loginPath in variable in routing. R.loginPath.
-- TODO No more hacky strings! THis allows us to change the naming conventions whenever we want
-- TODO utilize bulma more!
-- Tags: http://bulma.io/documentation/elements/tag/
-- Progress: http://bulma.io/documentation/elements/progress/
-- Notification:


homePageGameCard : String -> String -> String -> String -> String -> Html Msg
homePageGameCard gameSlug src_ title subtitle about =
    div
        [ class "card", style <| toStyle "border-radius:1em;" ]
        [ div
            [ class "card-image" ]
            [ figure
                [ class "image is-4by3" ]
                [ a [ href <| "/game/" ++ gameSlug, R.onLinkClick <| UpdateLocation <| "/game/" ++ gameSlug ]
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
                    [ p
                        [ class "title is-4" ]
                        [ text title
                        ]
                    , p
                        [ class "subtitle is-6" ]
                        [ text subtitle
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


accessDeniedPage : Model -> Html Msg
accessDeniedPage model =
    section []
        [ homePageHeader model
        , section
            [ class "section" ]
            [ div
                [ class "container" ]
                [ h1 [ class "title is-1" ] [ text "Access Restricted." ]
                , h3 [] [ text "Maybe you are in the wrong place?" ]
                ]
            ]
        ]


notFoundPage : Model -> Html Msg
notFoundPage model =
    section []
        [ homePageHeader model
        , section
            [ class "section" ]
            [ div
                [ class "container" ]
                [ h1 [ class "title is-1" ] [ text "Page Not Found (Oops!)" ]
                , h3 [] [ text "Maybe you are in the wrong place?" ]
                ]
            ]
        ]


badgesPage : Model -> Html Msg
badgesPage model =
    section []
        [ homePageHeader model
        , section
            [ class "section" ]
            [ div
                [ class "container" ]
                [ h1 [ class "title is-1" ] [ text "Badges" ]
                , h3 [] [ text "This feature is coming soon!" ]
                ]
            ]
        ]



-- TODO on badgesPage, use a Modal Card
-- http://bulma.io/documentation/components/modal/


settingsPage : Model -> Html Msg
settingsPage model =
    section []
        [ homePageHeader model
        , section
            [ class "section" ]
            [ div
                [ class "container" ]
                [ h1 [ class "title is-1" ] [ text "Settings" ]
                , h3 [] [ text "This feature is coming soon!" ]
                ]
            ]
        ]


gamePage : Model -> String -> Html Msg
gamePage model gameSlug =
    section []
        [ section
            [ class "section" ]
            [ div
                [ class "container" ]
                [ mainMenuButton
                , mainMenu model.activeRoute model.menuIsActive
                , h1 [ class "title is-1" ] [ text "Game" ]
                , h3 [] [ text <| "You are on game: " ++ gameSlug ]
                ]
            ]
        ]



-- TODO fetch data for game, indicate loading progress
-- TODO display stimuli and record user responses
-- TODO track partial data, attempt to save


mainMenuButton : Html Msg
mainMenuButton =
    a
        [ class "button is-primary is-large modal-button", attribute "data-target" "#menu", onClick <| MainMenu True ]
        [ text "MENU" ]


mainMenu : R.Route -> Bool -> Html Msg
mainMenu activeRoute_ menuIsActive =
    let
        class_ =
            if menuIsActive then
                "modal is-active"
            else
                "modal"
    in
        div
            [ class class_, id "menu" ]
            [ div
                [ class "modal-background" ]
                []
            , div
                [ class "modal-content" ]
                [ mainMenu_ activeRoute_ ]
            , button
                [ class "modal-close", onClick <| MainMenu False ]
                []
            ]


mainMenu_ : R.Route -> Html Msg
mainMenu_ activeRoute_ =
    div
        [ class "box" ]
        [ article
            [ class "media" ]
            [ div
                [ class "media-content" ]
                [ div
                    [ class "content" ]
                    [ mainMenuLink "Home" R.homePath (activeRoute_ == R.HomeRoute)
                    , mainMenuLink "Badges" R.badgesPath (activeRoute_ == R.BadgesRoute)
                    , mainMenuLink "Instructions" R.instructionsPath (activeRoute_ == R.InstructionsRoute)
                    , mainMenuLink "Settings" R.settingsPath (activeRoute_ == R.SettingsRoute)
                    ]
                ]
            ]
        ]


mainMenuLink : String -> String -> Bool -> Html Msg
mainMenuLink title linkPath active =
    let
        styles =
            if active then
                style_ "text-decoration:underline;font-weight:bold"
            else
                style []
    in
        h1
            []
            [ a [ styles, href <| linkPath, R.onLinkClick <| UpdateLocation linkPath ] [ text title ] ]



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



--link_ : String ->


link_ linkPath title children =
    a [ href <| linkPath, R.onLinkClick <| UpdateLocation linkPath ] children


logoSmall =
    link_ R.homePath "" [ img [ class "logo", src "img/logo.svg", style_ "max-width:60px;float:left;" ] [] ]
