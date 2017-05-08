module Game.View exposing (view, viewResult)

import Game exposing (BorderType(..))
import Game.Card
import Game.Result
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Markdown
import Ui.Card
import Numeral
import Svg exposing (Svg, svg, circle)
import Svg.Attributes as Svg exposing (cy, cx, r)


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
                    -- [ p [] [ text timer ]
                    -- , p [] [ text <| toString state.trialResult ]
                    [ case Game.Card.layout game of
                        Nothing ->
                            text ""

                        Just (Game.Info borderType string) ->
                            Ui.Card.middleBlock [ Markdown.toHtml [] string ]

                        Just (Game.Single borderType image) ->
                            viewSingleLayout borderType image

                        Just (Game.LeftRight borderType lImage rImage) ->
                            viewLeftRightLayout borderType lImage rImage

                        Just (Game.LeftOrRight borderType direction image) ->
                            viewLeftOrRightLayout borderType direction image

                        Just (Game.SelectGrid borderType rows cols images) ->
                            viewSelectGridLayout borderType rows cols images

                        Just (Game.RedCross borderType) ->
                            viewRedCross borderType

                        Just (Game.Fixation borderType) ->
                            viewFixation borderType

                        Just (Game.Probe borderType direction) ->
                            viewProbe borderType direction
                    ]

        Game.Finished state ->
            viewResult (Just state)
                { percentCorrect = Game.Result.percentCorrect state
                , averageResponseTimeResult = Game.Result.averageResponseTimeInMillisecond state
                }

        Game.NotPlaying ->
            div []
                [ a
                    [ class "button is-info is-large"
                    , onClick msg
                    ]
                    [ text "Start Game" ]
                ]


viewResult : Maybe Game.State -> { a | percentCorrect : Float, averageResponseTimeResult : Result String Float } -> Html msg
viewResult state { percentCorrect, averageResponseTimeResult } =
    let
        averageResponseTime =
            case averageResponseTimeResult of
                Err error ->
                    error

                Ok result ->
                    Numeral.format "0.00" result ++ " milliseconds"
    in
        Ui.Card.middleBlock
            [ h1 [ class "title" ] [ text "Results" ]
            , ul []
                [ li [] [ text <| "Average Response Time: " ++ averageResponseTime ]
                , li [] [ text <| "Percent Correct: " ++ Numeral.format "0.00" percentCorrect ++ "%" ]
                ]
            ]


border : BorderType -> List (Html msg) -> Html msg
border borderType content =
    case borderType of
        None ->
            div [ class "imageBox sized" ] content

        Grey ->
            div [ class "imageBox greyBorder sized" ] content

        Blue ->
            div [ class "imageBox blueBorder sized" ] content

        Black ->
            div [ class "imageBox solidBorder sized" ] content

        Dashed ->
            div [ class "imageBox dashedBorder sized" ] content


viewRedCross : BorderType -> Html msg
viewRedCross borderType =
    gameWrapper
        [ border borderType [ div [ class "redCross" ] [ text "X" ] ]
        ]


gameWrapper : List (Html msg) -> Html msg
gameWrapper game =
    div [ class "gameWrapper" ] game


viewSingleLayout : BorderType -> Game.Image -> Html msg
viewSingleLayout borderType image =
    gameWrapper [ border borderType [ img [ src image.url, class "squeezed" ] [] ] ]


viewLeftOrRightLayout : BorderType -> Game.Direction -> Game.Image -> Html msg
viewLeftOrRightLayout borderType direction image =
    gameWrapper
        [ border borderType
            [ img
                [ src image.url
                , case direction of
                    Game.Left ->
                        class "is-pulled-left squeezed"

                    Game.Right ->
                        class "is-pulled-right squeezed"
                ]
                []
            ]
        ]


viewSelectGridLayout : BorderType -> Int -> Int -> List Game.Image -> Html msg
viewSelectGridLayout borderType rows cols images =
    border borderType [ text (toString (rows * cols)) ]



-- DOT PROBE


viewLeftRightLayout : BorderType -> Game.Image -> Game.Image -> Html msg
viewLeftRightLayout borderType lImage rImage =
    div [ class "columns is-mobile" ]
        [ div [ class "column" ] [ img [ src lImage.url ] [] ]
        , div [ class "column" ] [ img [ src rImage.url ] [] ]
        ]


viewFixation : BorderType -> Html msg
viewFixation borderType =
    div
        [ class "columns is-mobile" ]
        [ div
            [ class "column" ]
            [ div [ class "fixationCross" ] [ text "+" ] ]
        ]


viewProbe : BorderType -> Game.Direction -> Html msg
viewProbe borderType direction =
    div
        [ class "columns is-mobile" ]
        (case direction of
            Game.Left ->
                [ div
                    [ class "column" ]
                    [ div [ class "probe" ] [ probe ] ]
                , div
                    [ class "column" ]
                    [ text "" ]
                ]

            Game.Right ->
                [ div
                    [ class "column" ]
                    [ text "" ]
                , div
                    [ class "column" ]
                    [ div [ class "probe" ] [ probe ] ]
                ]
        )


probe : Svg msg
probe =
    svg [ Svg.width "20", Svg.height "20" ] [ circle [ cx "10", cy "10", r "10" ] [] ]
