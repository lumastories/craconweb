module Admin.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (..)


-- TODO:  map AdminMsg to Msg in parent view


adminPage : Model -> Html AdminMsg
adminPage model =
    basicPage model
        [ p [] [ text <| "Welcome to the admin, " ++ model.jwtencoded ]
        , br [] []
        , button [ class "button", onClick FetchUsers ] [ text "fetch users" ]
        , p [] [ text <| toString model.adminModel.users ]
        , usersTable model
        , b [] [ text model.error ]
        ]


userRows users =
    let
        row user =
            tr []
                [ td [] [ text user.firstName ]
                , td [] [ text user.lastName ]
                , td [] [ text user.email ]
                , td [] [ button [] [ text "edit" ] ]
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



--[]
--[ tr
--    []
--    [ th
--        []
--        [ abbr
--            [ title "Name" ]
--            [ text "name"]
--        ]
--    ]
--]


basicPage : Model -> List (Html AdminMsg) -> Html AdminMsg
basicPage model children =
    section []
        [ h1 [] [ text "Admin" ]
        , section
            [ class "section" ]
            children
        ]
