module View exposing (view)

import Html exposing (..)
import Model exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- TODO add main menu with a link to instructions page (then, Badges, Settings, etc.)
-- TODO Create a new logo (brain with CC - craving control, crave control)
-- TODO Select a color pallette (check color blind compatability)
-- TODO Create reset password modal
-- TODO Write decoders and encoders for swagger
-- TODO Auto generate models, encoders and decoders from swagger (whaaaa) or use IDL or GRPC -> elm
-- TODO Rename the `dist` directory to something more fitting, check NoRedInk guidelines?


view : Model -> Html Msg
view model =
    let
        page =
            case model.activePage of
                Login ->
                    loginPage model

                Games ->
                    gamesPage model

                AccessDenied ->
                    text "nah nah nah, you can't be here."

                _ ->
                    text "umm..."
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
    section []
        [ div [ class "container" ]
            [ div [ class "columns is-desktop" ]
                [ div [ class "column is-half is-offset-one-quarter" ]
                    [ logo logoWidth
                    , loginBoxForm model
                    , p [ class "has-text-centered" ]
                        [ em [] [ text model.error ]
                        ]
                    ]
                ]
            ]
        ]


logoWidth : Int
logoWidth =
    200


loginButtonClass : Bool -> String
loginButtonClass spin =
    case spin of
        False ->
            "button is-dark"

        True ->
            "button is-dark is-loading"


loginBoxForm : Model -> Html Msg
loginBoxForm model =
    div [ class "box" ]
        [ Html.form
            [ onSubmit TryLogin
            , action "javascript:void(0);"
            ]
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
                [ button [ class <| loginButtonClass model.spin, onClick TryLogin ] [ text "Let's Go!" ]
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
                    [ text <| "Welcome, " ++ model.user.firstName ]
                  -- TODO save this in localStorage?
                , h2
                    [ class "subtitle" ]
                    [ text "Nice to see you" ]
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



-- TODO Add icons for games, link one to /#games/:id
-- TODO fetch data for game, indicate loading progress
-- TODO display stimuli and record user responses
-- TODO track partial data, attempt to save


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
        , gamesBody model
        ]



--classByPage : Page -> Page -> Attribute a
--classByPage page activePage =
--    classList
--        [ ( "item", True )
--        , ( "active", page == activePage )
--        ]
