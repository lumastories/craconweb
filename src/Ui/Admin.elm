module Ui.Admin
    exposing
        ( adminPage
        , registerPage
        , editUserPage
        , mesPage
        )

import Access as A
import Entity
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onSubmit, onCheck, onInput)
import Model exposing (Model, Msg(..), AdminModel)
import Routing as R
import Ui.Parts as Parts
import Ui.Card as C
import Dropdown exposing (Options)


adminPage : Model -> Html Msg
adminPage model =
    basicAdminPage model.glitching
        [ adminTop model.user
        , usersTable model
        ]


basicAdminPage : Maybe String -> List (Html Msg) -> Html Msg
basicAdminPage glitching children =
    section [ class "section" ]
        [ div
            [ class "container" ]
            ([ Parts.notification glitching "is-warning" ] ++ children)
        ]


mesPage : Model -> Html Msg
mesPage model =
    basicAdminPage model.glitching
        [ backButton
        , h1 [ class "title" ] [ text "Motivational Enhancement Statements" ]
        , p [ class "subtitle" ] [ text "Read and approve statements" ]
        , hr [] []
        , mesTable model.adminModel
        ]


publishButton : Bool -> String -> Html Msg
publishButton public id =
    case public of
        True ->
            a
                [ class "button is-danger is-outlined"
                , MesUnPublish id |> onClick
                ]
                [ text "Unpublish" ]

        False ->
            a
                [ class "button is-success is-outlined"
                , MesPublish id |> onClick
                ]
                [ text "Publish!" ]


mesTable : AdminModel -> Html Msg
mesTable am =
    case am.mesAnswers of
        Just mesAnswers ->
            mesAnswers
                |> List.map (\ms -> tr [] [ td [] [ text ms.essay ], td [] [ publishButton ms.public ms.id ] ])
                |> mesTableHelper

        Nothing ->
            C.middleBlock [ p [] [ text "No statements yet! Check back later." ] ]


mesTableHelper : List (Html Msg) -> Html Msg
mesTableHelper rows =
    table [ class "table is-bordered is-striped" ]
        [ thead []
            [ tr []
                [ th [] [ text "Essay" ], th [] [ text "Actions" ] ]
            ]
        , tbody [] rows
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
                , bButton "Approve MES" R.mesPath "is-link"
                , bButton "Go to games" R.homePath "is-link"
                , a ([ class "button is-link", onClick Logout ])
                    [ text "Logout" ]
                ]
            ]
        ]


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


editUserPage : Model -> String -> Html Msg
editUserPage model userid =
    case (A.userName model.users userid) of
        Just user ->
            editUser model.informing model.tasksrv user model.ugimgsets

        Nothing ->
            editUser404


usersTable : Model -> Html Msg
usersTable model =
    table [ class "table is-bordered is-striped is-narrow" ]
        [ thead []
            [ tr []
                [ th [] [ text "First Name" ]
                , th [] [ text "Last Name" ]
                , th [] [ text "Username" ]
                , th [] [ text "Email" ]
                , th [] [ text "Group" ]
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
                , td [] [ text user.groupId ]
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


bButton : String -> String -> String -> Html Msg
bButton title path mods =
    a
        [ class ("button " ++ mods)
        , href <| path
        , R.onLinkClick <| UpdateLocation path
        ]
        [ text title ]


editUser404 : Html Msg
editUser404 =
    basicAdminPage Nothing
        [ divColumns
            [ div [ class "column is-half is-offset-one-quarter" ]
                [ h1 [ class "title" ] [ text "User not found" ] ]
            ]
        ]


divColumns : List (Html Msg) -> Html Msg
divColumns children =
    div [ class "columns" ] children


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
                , Parts.notification informing "is-warning"
                , editUserForm tasksrv user
                ]
            ]
        ]


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
            [ label_ "First Name *"
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
            [ label_ "Username *"
            , textInput "username" "joey123"
            ]
        , div [ class "column" ]
            [ label_ "Email *"
            , emailInput "email" "joe@example.com"
            ]
        , div [ class "column" ]
            [ label_ "Password *"
            , textInput "password" "longfancyphrase"
            ]
        ]


dropdownOptions : Maybe String -> Maybe String -> Options Msg
dropdownOptions groupIdExp groupIdCon =
    let
        defaultOptions =
            Dropdown.defaultOptions GroupChanged

        items_ =
            case ( groupIdExp, groupIdCon ) of
                ( Just e, Just c ) ->
                    [ { value = e, text = "Experimental", enabled = True }
                    , { value = c, text = "Control", enabled = True }
                    ]

                _ ->
                    []
    in
        { defaultOptions
            | items = items_
            , emptyItem = Just { value = "0", text = "Select a group...", enabled = True }
        }


groupsDropDown : Model -> Html Msg
groupsDropDown model =
    Dropdown.dropdown
        (dropdownOptions model.groupIdExp model.groupIdCon)
        []
        (Just model.adminModel.tmpUserRecord.groupId)


registerUserForm : Model -> Html Msg
registerUserForm model =
    Html.form
        [ onSubmit TryRegisterUser ]
        [ firstLastReg
        , userEmailPassReg
        , div [ class "field" ]
            [ label
                [ class "label" ]
                [ text "Group *" ]
            , p
                [ class "control" ]
                [ span
                    [ class "select" ]
                    [ groupsDropDown model
                    ]
                ]
            ]
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
                ([ class "button is-link" ] ++ (Parts.linkAttrs R.adminPath))
                [ text "Cancel" ]
            ]


primaryButton : String -> String -> Html Msg
primaryButton title path =
    a
        [ class "button is-primary"
        , href <| path
        , R.onLinkClick <| UpdateLocation path
        ]
        [ text title ]


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
        , backButton
        ]


backButton : Html Msg
backButton =
    button
        ([ class "button is-link" ] ++ (Parts.linkAttrs R.adminPath))
        [ text "Go back" ]
