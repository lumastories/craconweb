module Ui.Admin exposing (adminPage)

import Access as A
import Entity
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Model exposing (Model, Msg(..))
import Routing as R
import Ui.Parts as Parts


adminPage : Model -> Html Msg
adminPage model =
    basicAdminPage model.glitching
        [ adminTop model.user
        , usersTable model
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
