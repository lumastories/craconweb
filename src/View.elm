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
                    gamePage model

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
                    "button is-purple"

                True ->
                    "button is-purple is-loading"
    in
        section []
            [ div [ class "container" ]
                [ div [ class "columns is-desktop" ]
                    [ div [ class "column is-half is-offset-one-quarter" ]
                        [ logo logoWidth
                        , div [ class "box purple" ]
                            [ label [ class "label" ]
                                [ text "Email" ]
                            , p [ class "control" ]
                                [ input [ class "input", placeholder "Email", type_ "email", onInput UpdateEmail ]
                                    []
                                ]
                            , label [ class "label" ]
                                [ text "Password" ]
                            , p [ class "control" ]
                                [ input [ class "input", placeholder "Password", type_ "password", onInput UpdatePassword ]
                                    []
                                ]
                            , hr []
                                []
                            , p [ class "control" ]
                                [ button [ class loginButtonClass, onClick Login ] [ text "Let's Go!" ]
                                ]
                            ]
                        , p [ class "has-text-centered" ]
                            [ em [] [ text model.error ]
                            ]
                        ]
                    ]
                ]
            ]


gamePage : Model -> Html Msg
gamePage model =
    section []
        [ div [ class "container" ]
            [ div [ class "columns is-desktop" ]
                [ div [ class "column" ] [ h1 [ class "title" ] [ text "Welcome, Guest" ] ]
                ]
            ]
        ]
