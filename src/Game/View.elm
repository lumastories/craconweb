module Game.View exposing (view)

import Game exposing (BorderType(..))
import Game.Card
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Markdown
import Ui.Card


view : Maybe (Game.Game msg) -> msg -> Html msg
view playingGame msg =
    case playingGame of
        Just game ->
            let
                state =
                    game |> Game.unwrap

                timer =
                    state.sessionStart
                        |> Maybe.map (\sessionStart -> state.currTime - sessionStart)
                        |> Maybe.map (\timer -> timer / 1000)
                        |> Maybe.map toString
                        |> Maybe.withDefault ""
            in
                div []
                    [ text timer
                    , case Game.Card.layout game of
                        Nothing ->
                            text ""

                        Just (Game.Info borderType string) ->
                            Ui.Card.middleBlock [ Markdown.toHtml [] string ]

                        Just (Game.Single borderType image) ->
                            border borderType [ img [ src image.url ] [] ]

                        Just (Game.LeftRight borderType lImage rImage) ->
                            border borderType [ text (lImage.url ++ rImage.url) ]

                        Just (Game.SelectGrid borderType rows cols images) ->
                            border borderType [ text (toString (rows * cols)) ]
                    ]

        Nothing ->
            div []
                [ a
                    [ class "button is-info is-large"
                    , onClick msg
                    ]
                    [ text "Start Game" ]
                ]


border : BorderType -> List (Html msg) -> Html msg
border borderType =
    case borderType of
        None ->
            div []

        Grey ->
            div [ class "greyBorder" ]

        Blue ->
            div [ class "blueBorder" ]

        Black ->
            div [ class "solidBorder" ]

        Dashed ->
            div [ class "dashedBorder" ]
