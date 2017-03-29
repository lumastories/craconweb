module Admin.Update exposing (update)

import Model exposing (..)
import Api
import Entity
import Http


update : AdminMsg -> Model -> ( Model, Cmd AdminMsg )
update msg model =
    case msg of
        UsersResponse (Ok usersList) ->
            ( { model | error = "no admin yet", adminModel = (setUsers model usersList.users) }, Cmd.none )

        UsersResponse (Err err) ->
            ( { model | error = toString err }, Cmd.none )

        FetchUsers ->
            let
                commands =
                    Cmd.batch [ Http.send UsersResponse (Api.getUsers model.api model.jwtencoded) ]
            in
                ( model, commands )


setUsers : Model -> List Entity.User -> AdminModel
setUsers model users_ =
    let
        adminModel =
            model.adminModel
    in
        { adminModel | users = users_ }
