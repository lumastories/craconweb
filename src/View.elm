module View exposing (view)

import Html exposing (..)
import Model exposing (..)
import Html.Attributes exposing (..)
import Material.Button as Button
import Material.Options as Options exposing (css)
import Material.Layout as Layout
import Material.Spinner as Loading
import Material.Textfield as Textfield
import FNV


view : Model -> Html Msg
view model =
    -- todo move case up
    Layout.render Mdl
        model.mdl
        []
        { header = []
        , drawer = []
        , tabs = ( [], [] )
        , main = [ viewBody model ]
        }

sizePx : Int -> String
sizePx size =
    (toString size) ++ "px"

logo : Int -> Html Msg
logo size =
    img [ src "img/logo.svg", style [ ( "max-width", sizePx size ) ] ] []


textfield : Model -> String -> (String -> Msg) -> String -> Html Msg
textfield model defVal msg label =
    Textfield.render Mdl
        [ FNV.hashString label ]
        model.mdl
        [ Textfield.label label
        , Options.onInput msg
        , Textfield.value defVal
        ]
        []



loginPage : Model -> Html Msg
loginPage model =
    let
        width =
            300
    in
    div [ style [ ( "margin", "0px auto" ), ( "width", sizePx width ) ] ]
        [ logo width
        , div [ style [ ( "padding", "1rem" ), ( "background", "white" ), ( "border-top", "1rem solid #eee" ) ] ]
            [ textfield model model.email UpdateEmail "Email"
            , Textfield.render Mdl
                [ FNV.hashString "Password" ]
                model.mdl
                [ Textfield.label "Password"
                , Options.onInput UpdatePassword
                , Textfield.value model.password
                , Textfield.password
                ]
                []
            , Button.render Mdl
                [ FNV.hashString "#games" ]
                model.mdl
                [ Button.raised
                , Button.colored
                , Button.link "#games"
                --, Options.onClick Noop
                ]
                [ text "Let's Go!" ]
            , Loading.spinner [ Loading.active model.spin ]
            ]
          -- , ul [] (List.map (\l -> (li [] [text l.hash])) model.history)
        ]


viewBody : Model -> Html Msg
viewBody model =
    let
        -- noise:
        page =
            case model.page of
                LoginPage ->
                    loginPage model

                _ ->
                    text "page coming soon"
    in
        div [ style [ ( "padding", "2rem" ) ] ]
            [ page
            ]
