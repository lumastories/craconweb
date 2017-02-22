module Update exposing (update)

import Model exposing (..)
import Api
import Http


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
                request =
                    Api.postCreds model

                cmd =
                    Http.send LoginResponse request

                l =
                    Debug.log "Update.Login" 1
            in
                ( { model | spin = True }, cmd )

        LoginResponse (Ok newToken) ->
            let
                l =
                    Debug.log "LoginResponse.Login" 1
            in
                ( { model | jwttoken = newToken, spin = False, page = GamePage }, Cmd.none )

        LoginResponse (Err err) ->
            let
                logger =
                    Debug.log "LoginResponse err" (toString err)
            in
                ( { model | spin = False, error = "Uh oh! Try again." }, Cmd.none )

        Presses code ->
            ( { model | presses = code :: model.presses }, Cmd.none )


page : String -> Page
page hash =
    case hash of
        "#games" ->
            GamePage

        "#badges" ->
            BadgePage

        _ ->
            LoginPage
