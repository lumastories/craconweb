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
                    , notification model.glitching "is-warning"
                    ]
                ]
            ]
        ]


loginPageButtonClass : ( Bool, String ) -> String
loginPageButtonClass loading =
    case loading of
        ( False, _ ) ->
            "button is-dark"

        ( True, _ ) ->
            "button is-dark is-loading"


loginPageBoxForm : Model -> Html Msg
loginPageBoxForm model =
    div [ class "box", marginS ]
        [ Html.form
            [ onSubmit TryLogin ]
            [ label [ class "label" ]
                [ text "Email" ]
            , p [ class "control" ]
                [ input
                    [ class "input"
                    , placeholder "Email"
                    , type_ "email"
                    , onInput UpdateEmail
                    ]
                    []
                ]
            , label [ class "label" ]
                [ text "Password" ]
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



-- ROUTING HELPERS


linkAttrs path =
    [ href <| path, R.onLinkClick <| UpdateLocation path ]



-- NAV BAR


navBar : Model -> Html Msg
navBar model =
    nav
        [ class "nav has-shadow" ]
        [ div [ class "container" ]
            [ div [ class "nav-left" ]
                [ logo "/"
                ]
            , navToggler model.isMenuActive
            , navRight model.isMenuActive model.activeRoute
            ]
        ]


navToggler : Bool -> Html Msg
navToggler activeMenu =
    span
        [ class <| "nav-toggle" ++ (isActive activeMenu), onClick MainMenuToggle ]
        [ span [] []
        , span [] []
        , span [] []
        ]


navRight activeMenu activeRoute =
    div
        [ id "nav-menu", class <| "nav-right nav-menu" ++ (isActive activeMenu) ]
        [ navLink "Badges" R.badgesPath (R.BadgesRoute == activeRoute)
        , navLink "Instructions" R.instructionsPath (R.InstructionsRoute == activeRoute)
        , a ([ class "nav-item is-tab", onClick Logout ] ++ linkAttrs "/login") [ text "Logout" ]
        ]


navLink text_ path active =
    let
        class_ =
            if active then
                "nav-item is-tab is-active"
            else
                "nav-item is-tab"
    in
        a ([ class class_ ] ++ (linkAttrs path))
            [ text text_ ]


logo linkPath =
    a
        [ class "nav-item"
        , href <| linkPath
        , R.onLinkClick <| UpdateLocation linkPath
        ]
        [ img [ src "img/logo.png" ] [] ]


isActive active =
    if active then
        " is-active"
    else
        ""



-- HOME PAGE


homePage : Model -> Html Msg
homePage model =
    div []
        [ navBar model
        , homePageBody model
        , notification model.glitching "is-danger"
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
            div [ class "column" ]
                [ homePageGameCard g.slug g.icon g.name g.dscript ]
    in
        List.map toCard
            [ model.gonogoGame
            , model.dotprobeGame
            , model.stopsignalGame
            , model.respondsignalGame
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
                [ a (linkAttrs gameSlug)
                    [ img
                        [ src <| "http://localhost:8654/repo/" ++ src_
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
                , small
                    []
                    [ text "..."
                    ]
                ]
            ]
        ]



-- The parent of basic pages.


notification ( isEnabled, content ) mods =
    if isEnabled then
        div
            [ class <| "notification " ++ mods ]
            [ button [ class "delete", onClick ResetNotifications ] []
            , text content
            ]
    else
        div [] []


basicPage : Model -> List (Html Msg) -> Html Msg
basicPage model children =
    section []
        [ navBar model
        , section
            [ class "section" ]
            children
        , notification model.glitching "is-danger"
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


goNoGoGame model =
    basicPage model
        [ div
            [ class "container" ]
            [ h1 [ class "title is-1" ] [ text model.gonogoGame.name ]
            , code [] [ model.gonogoGame |> toString |> text ]
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
            [ h1 [ class "title is-1" ]
                [ text "Dot Probe" ]
            , h3 [ class "subtitle is-3" ]
                [ text "work in progress" ]
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


instBlock title content =
    div [ class "column" ]
        [ p [ class "title" ] [ text title ]
        , p [] [ text content ]
        ]


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
                [ instBlock "Stop Signal" """You will see pictures presented
                     in either a dark blue or light gray border. Press the space
                      bar as quickly as you can. BUT only if you see a blue border
                       around the picture. Do not press if you see a grey border.
                        Go as fast as you can, but don’t sacrifice accuracy for speed."""
                , instBlock "Go/no-go" """You will see pictures either on
                    the left or right side of the screen, surrounded by a solid
                    or dashed border. Press ‘c’ when the picture is on the left
                    side of the screen or ‘m’ when the picture is on the right
                    side of the screen. BUT only if you see a solid bar around
                    the picture. Do not press if you see a dashed border. Go as
                    fast as you can, but don’t sacrifice accuracy for speed."""
                , instBlock "Respond signal" """You will see pictures on
                    the screen. Some of the pictures will be followed by a tone (a beep).
                        Please press the space bar as quickly as you can. BUT only
                        if you hear a beep after the picture. Do not press if you
                        do not hear a beep."""
                , instBlock "Dot probe" """You will see pictures on the
                    left and right side of the screen, followed by a dot on the
                    left or right side of the screen. Press the “c” if the dot is
                    on the left side of the screen or “m” when the dot is on the
                    right side of the screen. Go as fast as you can, but don’t
                    sacrifice accuracy for speed."""
                  -- TODO case switch on whther user is in the control group
                , instBlock "Visual search" """Food response training:
                        You will see a grid of 16 images of food. It is your job
                        to swipe on the image of the healthy food as quickly as
                        you can. Control training: you will see a grid of 16
                        images of birds and flowers. It is your job to swipe on
                        the image of the bird as quickly as you can. """
                ]
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
    p [ style [ ( "text-align", "center" ) ] ]
        [ img
            [ class "logo is-vcentered"
            , src "img/logo.svg"
            , style [ ( "max-width", "300px" ) ]
            ]
            []
        ]



{-

   ADMIN VIEWS

-}


textInput field_ placeholder_ =
    p [ class "control" ]
        [ input
            [ class "input"
            , placeholder placeholder_
            , type_ "text"
            , onInput <| SetRegistration field_
            ]
            []
        ]


emailInput field_ placeholder_ =
    p [ class "control" ]
        [ input
            [ class "input"
            , placeholder placeholder_
            , type_ "email"
            , onInput <| SetRegistration field_
            ]
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
            , textInput "username" "joey123"
            ]
        , div [ class "column" ]
            [ regLabel "Email"
            , emailInput "email" "joe@example.com"
            ]
        , div [ class "column" ]
            [ regLabel "Password"
            , textInput "password" "longfancyphrase"
            ]
        ]


groupDropDown : Maybe String -> Maybe String -> Html Msg
groupDropDown groupIdExp groupIdCon =
    case ( groupIdExp, groupIdCon ) of
        ( Just groupIdExp_, Just groupIdCon_ ) ->
            p [ class "control" ]
                [ span [ class "select" ]
                    [ select []
                        [ option [ value groupIdExp_ ]
                            [ text "Experimental" ]
                        , option [ value groupIdCon_ ]
                            [ text "Control" ]
                        ]
                    ]
                ]

        _ ->
            p [] []


registerUserForm model =
    Html.form
        [ onSubmit TryRegisterUser ]
        [ firstLastReg
        , userEmailPassReg
        , groupDropDown model.groupIdExp model.groupIdCon
        , hr []
            []
        , regButtons model.loading
        ]


regButtons ( loading, _ ) =
    div
        [ class "field is-grouped" ]
        [ button
            [ class <|
                "button is-primary "
                    ++ (if loading then
                            "is-loading"
                        else
                            ""
                       )
            ]
            [ text "Submit" ]
        , button
            ([ class "button is-link" ] ++ (linkAttrs R.adminPath))
            [ text "Cancel" ]
        ]


divColumns children =
    div [ class "columns" ] children


registerPage : Model -> Html Msg
registerPage model =
    basicAdminPage model
        [ divColumns
            [ div [ class "column is-half is-offset-one-quarter" ]
                [ h1
                    [ class "title" ]
                    [ text <| "Register a new user" ]
                , registerUserForm model
                ]
            ]
        ]


adminTop =
    div [ class "columns" ]
        [ div [ class "column" ]
            [ h1 [ class "title" ] [ text <| "Users" ] ]
        , div [ class "column" ]
            [ div [ class "block is-pulled-right" ]
                [ bButton "Register User" R.registerPath "is-success"
                , bButton "Go to games" R.homePath "is-link"
                ]
            ]
        ]


adminPage : Model -> Html Msg
adminPage model =
    basicAdminPage model
        [ adminTop
        , hr [] []
        , usersTable model
        ]


primaryButton : String -> String -> Html Msg
primaryButton title path =
    a
        [ class "button is-primary"
        , href <| path
        , R.onLinkClick <| UpdateLocation path
        ]
        [ text title ]


bButton : String -> String -> String -> Html Msg
bButton title path mods =
    a
        [ class ("button " ++ mods)
        , href <| path
        , R.onLinkClick <| UpdateLocation path
        ]
        [ text title ]


downloadButton =
    a
        [ class "button is-small" ]
        [ span
            [ class "icon is-small" ]
            [ i
                [ class "fa fa-file-text-o" ]
                []
            ]
        , span
            []
            [ text "Download CSV"
            ]
        ]



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
                , th [] [ text "Username" ]
                , th [] [ text "Email" ]
                , th [] [ text "Actions" ]
                ]
            ]
        , tbody [] (userRows model.users)
        ]


userRows users =
    let
        row user =
            tr []
                [ td [] [ text user.firstName ]
                , td [] [ text user.lastName ]
                , td [] [ text user.username ]
                , td [] [ text user.email ]
                , td []
                    [ downloadButton
                    ]
                ]
    in
        users
            |> List.map row


basicAdminPage : Model -> List (Html Msg) -> Html Msg
basicAdminPage model children =
    section [ class "section" ]
        [ div
            [ class "container" ]
            children
        , notification model.glitching "is-warning"
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
