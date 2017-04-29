module Ui.Admin exposing (adminPage, registerPage, editUserPage)

import Access as A
import Entity
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onSubmit, onCheck, onInput)
import Model exposing (Model, Msg(..))
import Routing as R
import Ui.Parts as Parts


adminPage : Model -> Html Msg
adminPage model =
    basicAdminPage model.glitching
        [ adminTop model.user
        , usersTable model
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
        , Parts.notification glitching "is-warning"
        ]


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
        , button
            ([ class "button is-link" ] ++ (Parts.linkAttrs R.adminPath))
            [ text "Go back" ]
        ]
