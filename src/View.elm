module View exposing (view)

import Update exposing (..)
import Model exposing (Model)
import Html exposing (..)
--import Html.Events exposing (..)
import Html.Attributes exposing (..)
--import Material
import Material.Button as Button
import Material.Options as Options exposing (css)
import Material.Layout as Layout
import Material.Spinner as Loading
--import Material.Progress as Loading
import Material.Textfield as Textfield
--import Material.Card as Card
import Material.Color as Color
import FNV

white : Options.Property c m
white =
  Color.text Color.white

view : Model -> Html Msg
view model =
    Layout.render Mdl
        model.mdl
        []
        { header = []
        , drawer = []
        , tabs = ( [], [] )
        , main = [ viewBody model ]
        }


-- used in the view
header : String -> Html Msg
header title = 
    h3 [ style [ ( "padding", ".5rem" ) ] ] [ text title ]


logo : String -> Html Msg
logo sizepx = 
    img [src "img/logo.svg", style [("max-width", sizepx)]] []

textfield : Model -> String -> (String -> Msg) -> String -> Html Msg
textfield model defVal msg label  =
    Textfield.render Mdl [FNV.hashString label] model.mdl
      [ Textfield.label label
      , Options.onInput msg
      , Textfield.value defVal
      ]
      []

 
passwordfield : Model -> String -> (String -> Msg) -> String -> Html Msg
passwordfield model defVal msg label  =
    Textfield.render Mdl [FNV.hashString label] model.mdl
      [ Textfield.label label
      , Options.onInput msg
      , Textfield.value defVal
      , Textfield.password 
      ]
      []


viewBody : Model -> Html Msg
viewBody model =
    let
        login =
            div [style [("margin", "0px auto"), ("width","300px"), ("background","white")]]
                [ logo "300px"
                , br [] []
                , textfield model model.email UpdateEmail "Email"
                , br [] []
                , passwordfield model model.password UpdatePassword "Password"
                , br [] []
                , Button.render Mdl
                    [ 0 ]
                    model.mdl
                    [ Button.raised
                    , Button.colored
                    , Button.link "#games"
                    , Options.onClick Authenticate

                    ]
                    [ text "Let's Go!" ]
                , br [] []
                , br [] []
                , Loading.spinner [ Loading.active model.spin ]
                ]
    in
        div [ style [ ( "padding", "2rem" ) ] ]
            [ login
            
            ]
