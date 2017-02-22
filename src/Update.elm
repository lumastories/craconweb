module Update exposing (update)

import Model exposing (..)
import Api
import Http
import Navigation
import Port


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateEmail newEmail ->
            ( { model | email = newEmail }, Cmd.none )

        UpdatePassword newPassword ->
            ( { model | password = newPassword }, Cmd.none )

        ChangePage location ->
            let
                newPage =
                    page location.hash
            in
                ( { model | history = location :: model.history, page = newPage }, Cmd.none )

        Login ->
            let
                cmd =
                    Http.send LoginResponse (Api.postCreds model)
            in
                ( { model | spin = True }, cmd )

        LoginResponse (Ok newToken) ->
            ( { model | jwttoken = newToken, spin = False, page = GamePage }, Cmd.batch [ Navigation.newUrl "#games", Port.jwtAuthSave newToken.token ] )

        LoginResponse (Err err) ->
            ( { model | spin = False, error = "Uh oh! Try again." }, Cmd.none )

        Presses code ->
            let
                hitEnter =
                    case code of
                        '\x0D' ->
                            True

                        _ ->
                            False

                cmd =
                    case hitEnter of
                        True ->
                            if model.page == LoginPage then
                                Http.send LoginResponse (Api.postCreds model)
                            else
                                Cmd.none

                        False ->
                            Cmd.none
            in
                ( { model | presses = code :: model.presses, spin = hitEnter }, cmd )


page : String -> Page
page hash =
    case hash of
        "#games" ->
            GamePage

        "#badges" ->
            BadgePage

        _ ->
            LoginPage
