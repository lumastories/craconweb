module Game.View exposing (view)

import Game exposing (BorderType(..))
import Game.Card
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Markdown
import Ui.Card


view : Game.GameState msg -> msg -> Html msg
view gameState msg =
    case gameState of
        Game.Playing game ->
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
                    [ p [] [ text timer ]
                    , p [] [ text <| toString state.trialResult ]
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

                        Just (Game.RedCross borderType) ->
                            border borderType [ redCross ]
                    ]

        Game.Finished state ->
            Ui.Card.middleBlock [ text <| toString state.log ]

        Game.NotPlaying ->
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


redCross : Html msg
redCross =
    div [ class "redCross" ] [ text "X" ]
