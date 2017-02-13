module Update exposing (..)

import Model exposing (Model)
import Material
import Navigation


type alias Slug =
    String


type Route
    = LoginRoute
    | GamesRoute
    | GameRoute Slug
    | BadgesRoute
    | BadgeRoute Slug
    | InstructionsRoute
    | SettingsRoute
    | LogoutRoute
    | NotFound


type Msg
    = UpdateEmail String
    | UpdatePassword String
    | Mdl (Material.Msg Msg)
    | UrlChange Navigation.Location
    | LetsGo String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateEmail newEmail ->
            ( { model | email = newEmail }, Cmd.none )

        UpdatePassword newPassword ->
            ( { model | password = newPassword }, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model

        UrlChange location ->
            ( { model | history = location :: model.history }, Cmd.none )

        LetsGo place ->
            ( model, Cmd.none )
