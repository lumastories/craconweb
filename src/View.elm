module View exposing (view)

import Entity
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (..)
import Routing as R


-- LOGIN PAGE


loginPage : Model -> Html Msg
loginPage model =
    section []
        [ div [ class "container" ]
            [ div [ class "columns is-desktop" ]
                [ div [ class "column is-half is-offset-one-quarter" ]
                    [ bigLogo
                    , loginPageBoxForm model
                    , p [ class "has-text-centered" ]
                        [ em [] [ text model.error ]
                        ]
                    ]
                ]
            ]
        ]


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
                [ button [ class <| loginPageButtonClass model.spin, type_ "submit" ] [ text "Let's Go!" ]
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


navToggler : Model -> Html Msg
navToggler model =
    let
        class_ =
            if model.menuIsActive then
                "nav-toggle is-active"
            else
                "nav-toggle"
    in
        span
            [ class class_, onClick MainMenuToggle ]
            [ span [] []
            , span [] []
            , span [] []
            ]


logo linkPath =
    a [ class "nav-item", href <| linkPath, R.onLinkClick <| UpdateLocation linkPath ] [ img [ src "img/logo.png" ] [] ]


navBarMenuLeftItems : Model -> List (Html Msg)
navBarMenuLeftItems model =
    let
        toItem i =
            navLink i.name model.activeRoute ( i.route, i.path ) "is-hidden-mobile"
    in
        (logo R.homePath) :: List.map toItem model.mainMenuItems


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
            , navToggler model
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
                , code [] [ model.games |> toString |> text ]
                , code [] [ model.gimages |> toString |> text ]
                ]
            ]


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


marginS : Attribute msg
marginS =
    style [ ( "margin", ".7em" ) ]


style_ : String -> Attribute msg
style_ rawStyles =
    style (toStyle rawStyles)


toStyle : String -> List ( String, String )
toStyle styles =
    -- "a:b;c:d;" >>> [("a", "b"), ("c","d")]
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


bigLogo : Html Msg
bigLogo =
    p [ style [ ( "text-align", "center" ) ] ] [ img [ class "logo is-vcentered", src "img/logo.svg", style [ ( "max-width", "300px" ) ] ] [] ]



{-

   ADMIN VIEWS

-}


textInput field_ placeholder_ =
    p [ class "control" ]
        [ input [ class "input", placeholder placeholder_, type_ "text", onInput <| SetRegistration field_ ]
            []
        ]


emailInput field_ placeholder_ =
    p [ class "control" ]
        [ input [ class "input", placeholder placeholder_, type_ "email", onInput <| SetRegistration field_ ]
            []
        ]


regLabel title =
    label [ class "label" ]
        [ text title ]


firstLastReg =
    div [ class "columns" ]
        [ div [ class "column" ]
            [ regLabel "First Name"
            , textInput "firstName" ""
            ]
        , div [ class "column" ]
            [ regLabel "Last Name"
            , textInput "lastName" ""
            ]
        ]


userEmailPassReg =
    div [ class "columns" ]
        [ div [ class "column" ]
            [ regLabel "Username"
            , textInput "username" "pick a fun username"
            ]
        , div [ class "column" ]
            [ regLabel "Email"
            , emailInput "email" "person@example.com"
            ]
        , div [ class "column" ]
            [ regLabel "Password"
            , textInput "password" "long and fancy password"
            ]
        ]


groupDropDown : Int -> Int -> Html Msg
groupDropDown expGroupId conGroupId =
    p [ class "control" ]
        [ span [ class "select" ]
            [ select []
                [ option [ value <| toString expGroupId ] [ text "Experimental" ]
                , option [ value <| toString conGroupId ] [ text "Control" ]
                ]
            ]
        ]


registerUserForm model =
    Html.form
        [ onSubmit TryRegisterUser ]
        [ firstLastReg
        , userEmailPassReg
        , groupDropDown model.adminModel.expGroupId model.adminModel.conGroupId
        , hr []
            []
        , p [ class "control" ]
            [ button [ class <| loginPageButtonClass model.spin, type_ "submit" ] [ text "Register" ]
            ]
        ]


registerPage : Model -> Html Msg
registerPage model =
    basicAdminPage model
        [ h1 [ class "title" ] [ text <| "Register User" ]
        , registerUserForm model
        ]


adminPage : Model -> Html Msg
adminPage model =
    basicAdminPage model
        [ h1 [] [ text <| "Welcome" ]
        , br [] []
        , primaryButton "Register User" R.registerPath
        , usersTable model
        ]


primaryButton : String -> String -> Html Msg
primaryButton title path =
    button [ class "button is-primary", href <| path, R.onLinkClick <| UpdateLocation path ] [ text title ]


userRows users =
    let
        row user =
            tr []
                [ td [] [ text user.firstName ]
                , td [] [ text user.lastName ]
                , td [] [ text user.email ]
                , td [] [ button [ class "button is-secondary" ] [ text "edit" ] ]
                ]
    in
        users
            |> List.map row



-- lastLogin
-- created
-- groupID
-- roles
-- link to edit


usersTable model =
    table [ class "table" ]
        [ thead []
            [ tr []
                [ th [] [ text "First Name" ]
                , th [] [ text "Last Name" ]
                , th [] [ text "Email" ]
                , th [] [ text "Actions" ]
                ]
            ]
        , tbody [] (userRows model.adminModel.users)
        ]


basicAdminPage : Model -> List (Html Msg) -> Html Msg
basicAdminPage model children =
    section []
        [ h1 [] [ text "Admin" ]
        , section
            [ class "section" ]
            children
        ]


view : Model -> Html Msg
view model =
    let
        page =
            case model.activeRoute of
                R.AdminRoute ->
                    adminPage model

                R.RegisterRoute ->
                    registerPage model

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
