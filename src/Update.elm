module Update exposing (..)

import Models exposing (Model)
import Material


type Msg
    = UpdateEmail String
    | UpdatePassword String
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateEmail newEmail ->
            ( { model | email = newEmail }, Cmd.none )

        UpdatePassword newPassword ->
            ( { model | password = newPassword }, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model
