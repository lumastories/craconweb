module View exposing (view)

import GameManager
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (..)
import Routing as R
import Entity
import Access as A


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
        [ div [ class "container" ]
            [ div [ class "columns is-desktop" ]
                [ div [ class "column is-half is-offset-one-quarter" ]
                    [ bigLogo model.filesrv
                    , loginPageBoxForm model
                    , notification model.glitching "is-warning"
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
                    , placeholder "Email"
                    , type_ "email"
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


linkAttrs : String -> List (Attribute Msg)
linkAttrs path =
    [ href <| path, R.onLinkClick <| UpdateLocation path ]


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


isAdmin : Visitor -> Bool
isAdmin visitor =
    case visitor of
        LoggedIn jwt ->
            List.map .name jwt.roles
                |> List.member "admin"

        _ ->
            False


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
    case isAdmin visitor of
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
        a ([ class class_ ] ++ (linkAttrs path))
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
                [ a (linkAttrs gameSlug)
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


notification : Maybe String -> String -> Html Msg
notification notifText mods =
    case notifText of
        Just nTxt ->
            div
                [ class <| "notification " ++ mods ]
                [ button [ class "delete", onClick ResetNotifications ] []
                , text nTxt
                ]

        Nothing ->
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
            [ poem404
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


poem404 : List (Html Msg) -> Html Msg
poem404 children =
    div
        [ class "columns" ]
        [ div
            [ class "column is-6 is-offset-3" ]
            [ div
                [ class "card" ]
                [ div
                    [ class "card-content" ]
                    children
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
            [ class "container" ]
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
game model title msg =
    let
        title_ =
            case model.playingGame of
                Nothing ->
                    h1 [ class "title is-1" ] [ text title ]

                _ ->
                    text ""
    in
        basicPage model
            [ div
                [ class "container" ]
                [ title_
                , gameView model.playingGame msg
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


gameView : Maybe Model.Game -> Msg -> Html Msg
gameView playingGame msg =
    case playingGame of
        Just (StopSignal data) ->
            div []
                [ GameManager.view data ]

        Just (GoNoGo data) ->
            div []
                [ GameManager.view data ]

        Just (DotProbe data) ->
            div []
                [ GameManager.view data ]

        Just (VisualSearch data) ->
            div []
                [ GameManager.view data ]

        Nothing ->
            div []
                [ a
                    [ class "button is-info is-large"
                    , onClick msg
                    ]
                    [ text "Start Game" ]
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
                [ instBlock "Go/no-go" """You will see pictures either on
                    the left or right side of the screen, surrounded by a solid
                    or dashed border. Press ‘c’ when the picture is on the left
                    side of the screen or ‘m’ when the picture is on the right
                    side of the screen. BUT only if you see a solid bar around
                    the picture. Do not press if you see a dashed border. Go as
                    fast as you can, but don’t sacrifice accuracy for speed."""
                , instBlock "Dot probe" """You will see pictures on the
                    left and right side of the screen, followed by a dot on the
                    left or right side of the screen. Press the “c” if the dot is
                    on the left side of the screen or “m” when the dot is on the
                    right side of the screen. Go as fast as you can, but don’t
                    sacrifice accuracy for speed."""
                , instBlock "Stop Signal" """You will see pictures presented
                     in either a dark blue or light gray border. Press the space
                      bar as quickly as you can. BUT only if you see a blue border
                       around the picture. Do not press if you see a grey border.
                        Go as fast as you can, but don’t sacrifice accuracy for speed."""
                , instBlock "Visual search" """Food response training:
                        You will see a grid of 16 images of food. It is your job
                        to swipe on the image of the healthy food as quickly as
                        you can."""
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



{-

   ADMIN VIEWS

-}


textInput : String -> String -> Html Msg
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


emailInput : String -> String -> Html Msg
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


label_ : String -> Html Msg
label_ title =
    label [ class "label" ]
        [ text title ]


firstLastReg : Html Msg
firstLastReg =
    div [ class "columns" ]
        [ div [ class "column" ]
            [ label_ "First Name"
            , textInput "firstName" ""
            ]
        , div [ class "column" ]
            [ label_ "Last Name"
            , textInput "lastName" ""
            ]
        ]


userEmailPassReg : Html Msg
userEmailPassReg =
    div [ class "columns" ]
        [ div [ class "column" ]
            [ label_ "Username"
            , textInput "username" "joey123"
            ]
        , div [ class "column" ]
            [ label_ "Email"
            , emailInput "email" "joe@example.com"
            ]
        , div [ class "column" ]
            [ label_ "Password"
            , textInput "password" "longfancyphrase"
            ]
        ]


toGroup : String -> String -> Bool -> Msg
toGroup groupIdCon_ groupIdExp_ bool =
    if bool then
        SetRegistration "exp" groupIdExp_
    else
        SetRegistration "con" groupIdCon_


groupDropDown : Maybe String -> Maybe String -> Html Msg
groupDropDown groupIdExp groupIdCon =
    case ( groupIdExp, groupIdCon ) of
        ( Just groupIdExp_, Just groupIdCon_ ) ->
            p [ class "control" ]
                [ label []
                    [ input [ type_ "checkbox", value groupIdExp_, onCheck (toGroup groupIdCon_ groupIdExp_) ] []
                    , text " Experimental"
                    ]
                ]

        _ ->
            p [] []


registerUserForm : Model -> Html Msg
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


regButtons : Maybe String -> Html Msg
regButtons loading =
    let
        class_ =
            case loading of
                Just l ->
                    "button is-primary is-loading"

                Nothing ->
                    "button is-primary"
    in
        div
            [ class "field is-grouped" ]
            [ button
                [ class class_ ]
                [ text "Submit" ]
            , button
                ([ class "button is-link" ] ++ (linkAttrs R.adminPath))
                [ text "Cancel" ]
            ]


divColumns : List (Html Msg) -> Html Msg
divColumns children =
    div [ class "columns" ] children


registerPage : Model -> Html Msg
registerPage model =
    basicAdminPage model.glitching
        [ divColumns
            [ div [ class "column is-half is-offset-one-quarter" ]
                [ h1
                    [ class "title" ]
                    [ text <| "Register a new user" ]
                , registerUserForm model
                ]
            ]
        ]


adminTop : Maybe Entity.User -> Html Msg
adminTop user =
    div [ class "columns" ]
        [ div [ class "column" ]
            [ h1 [ class "title" ] [ text "Users" ]
            ]
        , div [ class "column" ]
            [ div [ class "block is-pulled-right" ]
                [ bButton "Register User" R.registerPath "is-success"
                , bButton "Go to games" R.homePath "is-link"
                , a ([ class "button is-link", onClick Logout ])
                    [ text "Logout" ]
                ]
            ]
        ]


adminPage : Model -> Html Msg
adminPage model =
    basicAdminPage model.glitching
        [ adminTop model.user
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


iconButton : String -> String -> String -> String -> Html Msg
iconButton text_ path icon mods =
    a
        [ class <| "button " ++ mods
        , href <| path
        , R.onLinkClick <| UpdateLocation path
        ]
        [ span
            [ class <| "icon " ++ mods ]
            [ i
                [ class <| "fa " ++ icon ]
                []
            ]
        , span
            []
            [ text text_
            ]
        ]


usersTable : Model -> Html Msg
usersTable model =
    table [ class "table is-bordered is-striped is-narrow" ]
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


userRows : List Entity.User -> List (Html Msg)
userRows users =
    let
        row user =
            tr []
                [ td [] [ text user.firstName ]
                , td [] [ text user.lastName ]
                , td [] [ text user.username ]
                , td [] [ text user.email ]
                , td []
                    [ iconButton "Edit"
                        (R.editPath ++ user.id)
                        "fa-wrench"
                        "is-small"
                    ]
                ]
    in
        users
            |> List.map row


basicAdminPage : Maybe String -> List (Html Msg) -> Html Msg
basicAdminPage glitching children =
    section [ class "section" ]
        [ div
            [ class "container" ]
            children
        , notification glitching "is-warning"
        ]


editUserForm : String -> Entity.User -> Html Msg
editUserForm tasksrv user =
    Html.form
        [ enctype "multipart/form-data"
        , name "csvfile"
        , action <| tasksrv ++ "/upload/ugimgset"
        , method "POST"
        , id "csvForm"
        , class "box"
        ]
        [ input
            [ type_ "file"
            , id "csvFilInput"
            , accept ".csv"
            , name "upload"
            ]
            []
        , input
            [ type_ "hidden"
            , id "csvFilInput"
            , name "userid"
            , value user.id
            ]
            []
        , editButtons
        ]


editButtons : Html Msg
editButtons =
    div
        [ class "field is-grouped is-pulled-right" ]
        [ a
            [ class "button is-primary", onClick TryUpdateUser ]
            [ span
                [ class "icon" ]
                [ i
                    [ class "fa fa-file-text-o" ]
                    []
                ]
            , span
                []
                [ text "Upload"
                ]
            ]
        , button
            ([ class "button is-link" ] ++ (linkAttrs R.adminPath))
            [ text "Go back" ]
        ]


editUser : Maybe String -> String -> Entity.User -> Maybe (List Entity.Ugimgset) -> Html Msg
editUser informing tasksrv user ugimgsets =
    basicAdminPage Nothing
        [ divColumns
            [ div [ class "column is-half is-offset-one-quarter" ]
                [ h1
                    [ class "title" ]
                    [ text "Upload valuations for "
                    , strong [] [ text user.firstName ]
                    ]
                , br [] []
                , notification informing "is-warning"
                , editUserForm tasksrv user
                , prevUgimgsets ugimgsets
                ]
            ]
        ]


prevUgimgsets : Maybe (List Entity.Ugimgset) -> Html Msg
prevUgimgsets ugimgsets =
    div [ class "box" ] [ text "Previous uploads" ]


editUser404 : Html Msg
editUser404 =
    basicAdminPage Nothing
        [ divColumns
            [ div [ class "column is-half is-offset-one-quarter" ]
                [ h1 [ class "title" ] [ text "User not found" ] ]
            ]
        ]


editUserPage : Model -> String -> Html Msg
editUserPage model userid =
    case (A.userName model.users userid) of
        Just user ->
            editUser model.informing model.tasksrv user model.ugimgsets

        Nothing ->
            editUser404


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
                    stopSignalGame model

                R.EditUserRoute userid ->
                    editUserPage model userid
    in
        div [] [ page ]
