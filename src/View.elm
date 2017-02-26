module View exposing (view)

import Html exposing (..)
import Model exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


view : Model -> Html Msg
view model =
    let
        page =
            case model.page of
                LoginPage ->
                    loginPage model

                GamePage ->
                    gamesPage model

                _ ->
                    text "page coming soon"
    in
        div [] [ page ]


sizePx : Int -> String
sizePx size =
    (toString size) ++ "px"


centerStyled : Attribute msg
centerStyled =
    style [ ( "text-align", "center" ) ]


logo : Int -> Html Msg
logo size =
    p [ centerStyled ] [ img [ class "logo is-vcentered", src "img/logo.svg", style [ ( "max-width", sizePx size ) ] ] [] ]


loginPage : Model -> Html Msg
loginPage model =
    let
        logoWidth =
            200

        loginButtonClass =
            case model.spin of
                False ->
                    "button is-dark"

                True ->
                    "button is-dark is-loading"
    in
        section []
            [ div [ class "container" ]
                [ div [ class "columns is-desktop" ]
                    [ div [ class "column is-half is-offset-one-quarter" ]
                        [ logo logoWidth
                        , div [ class "box" ]
                            [ label [ class "label" ]
                                [ text "Email" ]
                            , p [ class "control" ]
                                [ input [ class "input", placeholder "Email", type_ "email", onInput LoginEmail ]
                                    []
                                ]
                            , label [ class "label" ]
                                [ text "Password" ]
                            , p [ class "control" ]
                                [ input [ class "input", placeholder "Password", type_ "password", onInput LoginPassword ]
                                    []
                                ]
                            , hr []
                                []
                            , p [ class "control" ]
                                [ button [ class loginButtonClass, onClick LoginSend ] [ text "Let's Go!" ]
                                ]
                            ]
                        , p [ class "has-text-centered" ]
                            [ em [] [ text model.error ]
                            ]
                        ]
                    ]
                ]
            ]


gamesHeader : Model -> Html Msg
gamesHeader model =
    section
        [ class "hero" ]
        [ div
            [ class "hero-body" ]
            [ div
                [ class "container" ]
                [ h1
                    [ class "title" ]
                    [ text <| "Welcome, " ++ model.jwttoken.token ]
                , h2
                    [ class "subtitle" ]
                    [ text "Nice to see you" ]
                ]
            ]
        ]


gamesBodyHeader : Model -> Html Msg
gamesBodyHeader model =
    section
        [ class "section" ]
        [ div
            [ class "container" ]
            [ div
                [ class "heading" ]
                [ h1
                    [ class "title" ]
                    [ text "Games" ]
                , h2
                    [ class "subtitle" ]
                    [ text "These should be fun"
                    ]
                ]
            ]
        ]


gamesGrid : Model -> Html Msg
gamesGrid model =
    div [ class "columns" ]
        [ div [ class "column" ] [ text "Visual Search" ]
        , div [ class "column" ] [ text "Dot Probe" ]
        , div [ class "column" ] [ text "Go/No-Go" ]
        , div [ class "column" ] [ text "Stop Signal" ]
        , div [ class "column" ] [ text "Respond Signal" ]
        ]


gamesBody : Model -> Html Msg
gamesBody model =
    section
        [ class "section" ]
        [ div
            [ class "container" ]
            [ gamesGrid model ]
        ]


gamesPage : Model -> Html Msg
gamesPage model =
    section []
        [ gamesHeader model
        , gamesBodyHeader model
        , gamesBody model
        ]
