module Ui.Card exposing (middleBlock)

import Html exposing (..)
import Html.Attributes exposing (..)


middleBlock : List (Html msg) -> Html msg
middleBlock children =
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
