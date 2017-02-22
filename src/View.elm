module View exposing (view)

import Html exposing (..)
import Model exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


view : Model -> Html Msg
view model =
    div [] [viewBody model]


sizePx : Int -> String
sizePx size =
    (toString size) ++ "px"

sizeEm : Int -> String
sizeEm size =
    (toString size) ++ "em"

sizeRem : Int -> String
sizeRem size =
    (toString size) ++ "rem"


centerStyled = 
    style [("text-align", "center")]

logo : Int -> Html Msg
logo size =
    p [centerStyled] [img [ class "logo is-vcentered", src "img/logo.svg", style [ ( "max-width", sizePx size ) ] ] []]

maxwidth : Int -> Attribute msg
maxwidth size = 
    style [ ( "max-width", sizeEm size ) ]

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
        section [] [
            div [ class "container" ]
                [ div [ class "columns is-desktop" ]
                    [ div [ class "column is-half is-offset-one-quarter" ]
                        [ logo logoWidth 
                        , div [ class "box" ]
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
                                [ button [ class loginButtonClass, onClick Login, href "#games" ] [ text "Let's Go!" ]
                                ]
                            ]
                        , p [ class "has-text-centered" ]
                            [ em [] [ text model.error ]
                            ]
                        ]
                    ]
                ]
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
