module Admin.Update exposing (update)

import Model exposing (..)


update : AdminMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UsersResponse (Ok users) ->
            ( { model | error = "no admin yet" }, Cmd.none )

        UsersResponse (Err err) ->
            ( { model | error = "no admin yet" }, Cmd.none )
