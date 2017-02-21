module Update exposing (update)

import Model exposing (..)
import Material
import Api
import Http


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateEmail newEmail ->
            ( { model | email = newEmail }, Cmd.none )

        UpdatePassword newPassword ->
            ( { model | password = newPassword }, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model

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
            in
                ( { model | spin = True}, cmd )

        LoginResponse (Ok newToken) ->
            ( { model | jwttoken = newToken, spin=False}, Cmd.none )

        LoginResponse (Err err) ->
            let
                logger =
                    Debug.log (toString err) "LoginResponse err"
            in 
                ({ model | spin = False}, Cmd.none )


page : String -> Page
page hash =
    case hash of
        "#games" ->
            GamePage

        "#badges" ->
            BadgePage

        _ ->
            LoginPage
