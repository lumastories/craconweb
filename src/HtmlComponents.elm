module HtmlComponents exposing (..)
import Html exposing (..)
import Model exposing (..)
import Html.Attributes exposing (..)
import Material.Button as Button
import Material.Options as Options exposing (css)
import Material.Layout as Layout
import Material.Spinner as Loading
import Material.Textfield as Textfield
import Material.Color as Color
import FNV

rootView : Model -> Html Msg
rootView model =
    Layout.render Mdl
        model.mdl
        []
        { header = []
        , drawer = []
        , tabs = ( [], [] )
        , main = [ viewBody model ]
        }

white : Options.Property c m
white =
  Color.text Color.white

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

loginPage : Model -> Html Msg
loginPage model =
    div [style [("margin", "0px auto"), ("width","300px")]]
        [ logo "300px"
        , div [style [("padding", "1rem"), ("background","white"), ("border-top", "1rem solid #eee")]] [
            textfield model model.email UpdateEmail "Email"
            , passwordfield model model.password UpdatePassword "Password"
            , Button.render Mdl [ FNV.hashString "#games" ] model.mdl
            [ Button.raised
            , Button.colored
            , Button.link "#games"
            , Options.onClick Noop
            ]
            [ text "Let's Go!" ]
            , Loading.spinner [ Loading.active model.spin ]
        ]
        -- , ul [] (List.map (\l -> (li [] [text l.hash])) model.history)
        ]

viewBody : Model -> Html Msg
viewBody model =
    let
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

