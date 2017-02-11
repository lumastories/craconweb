module View exposing (view)

import Update exposing (..)
import Models exposing (Model)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Material
import Material.Scheme
import Material.Button as Button
import Material.Options as Options exposing (css)
import Material.Layout as Layout
import Material.Spinner as Loading
import Material.Progress as Loading
import Material.Textfield as Textfield


view : Model -> Html Msg
view model =
    Layout.render Mdl
        model.mdl
        [ Layout.fixedHeader ]
        { header = [ h1 [ style [ ( "padding", "2rem" ) ] ] [ text "Login" ] ]
        , drawer = []
        , tabs = ( [], [] )
        , main = [ viewBody model ]
        }
        |> Material.Scheme.top


viewBody : Model -> Html Msg
viewBody model =
    let
        login =
            div []
                [ Textfield.render Mdl
                    [ 2 ]
                    model.mdl
                    [ Textfield.label "Enter Email"
                    , Textfield.floatingLabel
                    , Textfield.text_
                    ]
                    []
                , Textfield.render Mdl
                    [ 5 ]
                    model.mdl
                    [ Textfield.label "Enter Password"
                    , Textfield.floatingLabel
                    , Textfield.password
                    ]
                    []
                ]
    in
        div [ style [ ( "padding", "2rem" ) ] ]
            [ Loading.indeterminate
            , Loading.spinner
                [ Loading.active True ]
            , login
            , Button.render Mdl
                [ 1 ]
                model.mdl
                []
                [ text "Let's Go!" ]
            ]
