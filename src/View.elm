module View exposing (view)

import Game.View as Game
import Game
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (..)
import Routing as R
import Entity
import Ui.Card as Card
import Ui.Admin as Admin
import Ui.Parts as Parts
import List.Extra


bigLogo : String -> Html Msg
bigLogo filesrv =
    p [ style [ ( "text-align", "center" ) ] ]
        [ img
            [ class "logo is-vcentered"
            , src (filesrv ++ "/repo/logo.svg")
            , style [ ( "max-width", "300px" ) ]
            ]
            []
        ]


loginPage : Model -> Html Msg
loginPage model =
    section []
        [ div [ class "container scale-in-center" ]
            [ div [ class "columns is-desktop" ]
                [ div [ class "column is-half is-offset-one-quarter" ]
                    [ bigLogo model.filesrv
                    , loginPageBoxForm model
                    , Parts.notification model.glitching "is-warning"
                    ]
                ]
            ]
        ]


loginPageButtonClass : Maybe String -> String
loginPageButtonClass loading =
    case loading of
        Just _ ->
            "button is-fullwidth is-success is-loading"

        Nothing ->
            "button is-fullwidth is-success"


loginPageBoxForm : Model -> Html Msg
loginPageBoxForm model =
    div [ class "box", marginS ]
        [ Html.form
            [ onSubmit TryLogin ]
            [ p [ class "control" ]
                [ input
                    [ class "input"
                    , placeholder "Username"
                    , type_ "text"
                    , onInput UpdateEmail
                    ]
                    []
                ]
            , br [] []
            , p [ class "control" ]
                [ input
                    [ class "input"
                    , placeholder "Password"
                    , type_ "password"
                    , onInput UpdatePassword
                    ]
                    []
                ]
            , hr []
                []
            , p [ class "control" ]
                [ button
                    [ class <| loginPageButtonClass model.loading
                    , type_ "submit"
                    ]
                    [ text "Let's Go!" ]
                ]
            ]
        ]


navBar : Model -> Html Msg
navBar model =
    nav
        [ class "nav has-shadow" ]
        [ div [ class "container" ]
            [ div [ class "nav-left" ]
                [ tinyLogo model.filesrv
                ]
            , navToggler model.isMenuActive
            , navRight model.isMenuActive model.activeRoute model.visitor
            ]
        ]


isPowerful : Visitor -> Bool
isPowerful visitor =
    case visitor of
        LoggedIn jwt ->
            List.map .name jwt.roles
                |> isAdminOrStaff

        _ ->
            False


isAdminOrStaff : List String -> Bool
isAdminOrStaff roles =
    (List.member "admin" roles) || (List.member "staff" roles)


navToggler : Bool -> Html Msg
navToggler activeMenu =
    span
        [ class <| "nav-toggle" ++ (isActive activeMenu), onClick MainMenuToggle ]
        [ span [] []
        , span [] []
        , span [] []
        ]


adminLink : Visitor -> Html Msg
adminLink visitor =
    case isPowerful visitor of
        True ->
            navLink "Admin" R.adminPath False

        False ->
            div [] []


navRight : Bool -> R.Route -> Visitor -> Html Msg
navRight activeMenu activeRoute visitor =
    div
        [ id "nav-menu", class <| "nav-right nav-menu" ++ (isActive activeMenu) ]
        [ navLink "Games" R.homePath (R.HomeRoute == activeRoute)
        , navLink "Badges" R.badgesPath (R.BadgesRoute == activeRoute)
        , navLink "Instructions" R.instructionsPath (R.InstructionsRoute == activeRoute)
        , navLink "Statements" R.statementsPath (R.StatementsRoute == activeRoute)
        , adminLink visitor
        , a ([ class "nav-item is-tab", onClick Logout ]) [ text "Logout" ]
        ]


navLink : String -> String -> Bool -> Html Msg
navLink text_ path active =
    let
        class_ =
            if active then
                "nav-item is-tab is-active"
            else
                "nav-item is-tab"
    in
        a ([ class class_ ] ++ (Parts.linkAttrs path))
            [ text text_ ]


tinyLogo : String -> Html Msg
tinyLogo filesrv =
    a
        [ class "nav-item"
        , href <| R.homePath
        , R.onLinkClick <| UpdateLocation R.homePath
        ]
        [ img [ src (filesrv ++ "/repo/logo.png") ] [] ]


isActive : Bool -> String
isActive active =
    if active then
        " is-active"
    else
        ""


basicPage : Model -> List (Html Msg) -> Html Msg
basicPage model children =
    section []
        [ mesQueryModal model.mesQuery model.request
        , navBar model
        , section
            [ class "section" ]
            children
        , Parts.notification model.glitching "is-danger"
        ]


homePage : Model -> Html Msg
homePage model =
    div []
        [ mesQueryModal model.mesQuery model.request
        , navBar model
        , homePageBody model
        , Parts.notification model.glitching "is-danger"
        , loading_screen model.domLoaded
        ]


loading_screen : Bool -> Html msg
loading_screen is_loading =
    case is_loading of
        False ->
            Parts.modal [ section [ class "modal-card-body" ] [ h1 [ class "title" ] [ text "Loading games...", i [ class "fa fa-loading fa-spin" ] [] ] ] ]

        True ->
            text ""


homePageBody : Model -> Html Msg
homePageBody model =
    section
        [ class "hero is-primary" ]
        [ div
            [ class "hero-body" ]
            [ div
                [ class "container" ]
                [ h1 [ class "title is-1" ] [ (welcome model.user) ]
                , homePageGrid model
                ]
            ]
        ]


welcome : Maybe Entity.User -> Html msg
welcome user =
    case user of
        Just u ->
            (text <| "Welcome, " ++ u.firstName)

        Nothing ->
            (text "")


homePageGrid : Model -> Html Msg
homePageGrid model =
    div [ class "columns" ]
        (homePageGameCards model)


homePageGameCards : Model -> List (Html Msg)
homePageGameCards model =
    let
        toCard game =
            case game of
                Just g ->
                    div [ class "column" ]
                        [ homePageGameCard g.slug (model.filesrv ++ "/repo/" ++ g.icon) g.name g.dscript ]

                Nothing ->
                    div [] []
    in
        List.map toCard
            [ model.gonogoGame
            , model.dotprobeGame
            , model.stopsignalGame
            , model.visualsearchGame
            ]


homePageGameCard : String -> String -> String -> String -> Html Msg
homePageGameCard gameSlug src_ title about =
    div
        [ class "card", style <| toStyle "border-radius:1em;" ]
        [ div
            [ class "card-image" ]
            [ figure
                [ class "image is-4by3" ]
                [ a (Parts.linkAttrs gameSlug)
                    [ img
                        [ src src_
                        , alt title
                        ]
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
                ]
            ]
        ]


mesQueryModal : Maybe String -> Maybe String -> Html Msg
mesQueryModal q feedback =
    case q of
        Nothing ->
            text ""

        Just q ->
            Parts.modal
                [ div
                    [ class "field" ]
                    [ label
                        [ class "label white title is-3" ]
                        [ text q ]
                    , p
                        [ class "control" ]
                        [ textarea
                            [ class "textarea"
                            , placeholder "Answer the above question. Please sign with your first and last initial."
                            , onInput UpdateMesAnswer
                            ]
                            []
                        ]
                    , p [ class "questionFeedback" ] [ Maybe.withDefault "" feedback |> text ]
                    , button
                        [ class "button is-primary is-large"
                        , onClick TrySubmitMesAnswer
                        ]
                        [ text "Share" ]
                    ]
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
            [ Card.middleBlock
                [ h1 [ class "title is-1" ] [ text "Poem 404" ]
                , h5 [ class "subtitle is-5" ] [ text "Page Not Found" ]
                , p []
                    [ text "I shall be telling this with a sigh"
                    , br [] []
                    , text "Somewhere ages and ages hence:"
                    , br [] []
                    , text "Two roads diverged in a wood, and I-"
                    , br [] []
                    , text "I took the one less traveled by,"
                    , br [] []
                    , text "And that has made all the difference."
                    , br [] []
                    ]
                , em [] [ text "- Robert Frost" ]
                , br [] []
                ]
            ]
        ]


badge : String -> Html Msg
badge text_ =
    p [] [ i [ class "fa fa-certificate fa-6" ] [], text text_ ]


badgesPage : Model -> Html Msg
badgesPage model =
    basicPage model
        [ div
            [ class "container swing-in-top-fwd" ]
            [ h1 [ class "title is-1" ] [ text "Badges" ]
            , div []
                [ div [ class "columns" ]
                    [ div [ class "column" ]
                        [ div [ class "box badge1" ]
                            [ h1 [ class "title is-1" ] [ text "Log in ", i [ class "fa fa-unlock" ] [] ]
                            , p [] [ text "You logged on to the site, congratulations." ]
                            ]
                        ]
                    , div [ class "column" ]
                        [ div [ class "box badge2" ]
                            [ h1 [ class "title is-1" ] [ text "90% ", i [ class "fa fa-lock" ] [] ]
                            , p [] [ text "Reach 90% accuracy to unlock this badge!" ]
                            ]
                        ]
                    , div [ class "column" ]
                        [ div [ class "box badge3" ]
                            [ h1 [ class "title is-1" ] [ text "80% ", i [ class "fa fa-lock" ] [] ]
                            , p [] [ text "Reach 80% accuracy to unlock this badge!" ]
                            ]
                        ]
                    ]
                , div
                    [ class "columns" ]
                    [ div [ class "column" ]
                        [ div [ class "box badge4" ]
                            [ h1 [ class "title is-1" ] [ text "Try all games ", i [ class "fa fa-lock" ] [] ]
                            , p [] [ text "Try all the games to earn this" ]
                            ]
                        ]
                    , div [ class "column" ]
                        [ div [ class "box badge5" ]
                            [ h1 [ class "title is-1" ] [ text "3-day streak ", i [ class "fa fa-lock" ] [] ]
                            , p [] [ text "Play three days in a row to earn this one." ]
                            ]
                        ]
                    , div [ class "column" ]
                        [ div [ class "box badge6" ]
                            [ h1 [ class "title is-1" ] [ text "7-day streak ", i [ class "fa fa-lock" ] [] ]
                            , p [] [ text "Play a game 7 days in a row to earn this badge!" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


settingsPage : Model -> Html Msg
settingsPage model =
    basicPage model
        [ div
            [ class "container" ]
            [ h1 [ class "title is-1" ] [ text "Settings" ]
            , h3 [] [ text "This feature is coming soon!" ]
            ]
        ]


game : Model -> String -> Msg -> Html Msg
game model title initMsg =
    let
        title_ =
            case model.gameState of
                Game.NotPlaying ->
                    h1 [ class "title is-1" ] [ text title ]

                Game.Loading _ _ ->
                    h1 [ class "title is-1" ] [ text title ]

                _ ->
                    text ""
    in
        basicPage model
            [ div
                [ class "container" ]
                [ title_
                , case model.activeRoute of
                    R.GameRouteSs ->
                        Game.view { gameState = model.gameState, initMsg = initMsg, intIndicationMsg = IntIndication }

                    R.GameRouteGn ->
                        Game.view { gameState = model.gameState, initMsg = initMsg, intIndicationMsg = IntIndication }

                    R.GameRouteDp ->
                        Game.view { gameState = model.gameState, initMsg = initMsg, intIndicationMsg = IntIndication }

                    R.GameRouteVs ->
                        Game.view { gameState = model.gameState, initMsg = initMsg, intIndicationMsg = IntIndication }

                    _ ->
                        text "Invalid Game"
                ]
            ]


visualSearchGame : Model -> Html Msg
visualSearchGame model =
    game model "Visual Search" InitVisualSearch


dotProbeGame : Model -> Html Msg
dotProbeGame model =
    game model "Dot Probe" InitDotProbe


goNoGoGame : Model -> Html Msg
goNoGoGame model =
    game model "Go/No Go" InitGoNoGo


stopSignalGame : Model -> Html Msg
stopSignalGame model =
    game model "Stop Signal" InitStopSignal


instructionsPage : Model -> Html Msg
instructionsPage model =
    basicPage model
        [ div
            [ class "container" ]
            [ h1 [ class "title is-1" ]
                [ text "Instructions" ]
            , hr [] []
            , div
                [ class "columns" ]
                [ instBlock "Go/no-go" """You will see pictures either on
                    the left or right side of the screen, surrounded by a solid
                    or dashed border. Press 'c' when the picture is on the left
                    side of the screen or 'm' when the picture is on the right
                    side of the screen. BUT only if you see a solid bar around
                    the picture. Do not press if you see a dashed border. Go as
                    fast as you can, but don't sacrifice accuracy for speed."""
                , instBlock "Dot probe" """You will see pictures on the
                    left and right side of the screen, followed by a dot on the
                    left or right side of the screen. Press the "c" if the dot is
                    on the left side of the screen or "m" when the dot is on the
                    right side of the screen. Go as fast as you can, but don't
                    sacrifice accuracy for speed."""
                , instBlock "Stop Signal" """You will see pictures presented
                     in either a dark blue or light gray border. Press the space
                      bar if you see a blue border around the picture.
                    Do not press if you see a grey border.
                        Go as fast as you can, but don't sacrifice accuracy for speed."""
                , instBlock "Visual search" """You will see a grid of images.
                    Select the target image as quickly as you can. Don't sacrifice
                    accuracy for speed."""
                ]
            ]
        ]


marginS : Attribute msg
marginS =
    style [ ( "margin", ".7em" ) ]


style_ : String -> Attribute msg
style_ rawStyles =
    style (toStyle rawStyles)


instBlock : String -> String -> Html Msg
instBlock title content =
    div [ class "column" ]
        [ p [ class "title" ] [ text title ]
        , p [] [ text content ]
        ]


toStyle : String -> List ( String, String )
toStyle styles =
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


statementsPage : Model -> Html Msg
statementsPage model =
    basicPage model
        (statements model.statements)


statements : Maybe (List MesAnswer) -> List (Html Msg)
statements mesAnswers =
    case mesAnswers of
        Nothing ->
            [ Card.middleBlock
                [ h1 [ class "title" ] [ text "Statements" ]
                , p [] [ text """Coming soon! You will be able to see personal
                            statements from other members of the study about their journey to better
                            health and some of the choices they made that helped them get there!""" ]
                ]
            ]

        Just mesAnswers ->
            [ h1 [ class "title" ] [ text "Statements" ] ]
                ++ (List.map (div [ class "columns" ]) (List.map statement mesAnswers |> List.Extra.greedyGroupsOf 4))


statement : MesAnswer -> Html Msg
statement mes =
    div [ class "column" ]
        [ blockquote []
            [ i [ class "fa fa-quote-left" ] []
            , p [] [ text mes.essay ]
            , i [ class "fa fa-quote-right" ] []
            ]
        ]



{-

   ADMIN VIEWS

-}


view : Model -> Html Msg
view model =
    let
        page =
            case model.activeRoute of
                R.AdminRoute ->
                    Admin.adminPage model

                R.StatementsRoute ->
                    statementsPage model

                R.RegisterRoute ->
                    Admin.registerPage model

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
                    stopSignalGame model

                R.EditUserRoute userid ->
                    Admin.editUserPage model userid

                R.MesRoute ->
                    Admin.mesPage model
    in
        div [] [ page ]
