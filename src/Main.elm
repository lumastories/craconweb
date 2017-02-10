module Main exposing (..)
import Html exposing (..)

-- Model & init
type alias Model =
    { things : List String
    }

init : ( Model, Cmd Msg )
init =
    ( Model [], Cmd.none )


-- View
view : Model -> Html Msg
view model =
    text "hello"


-- Update
type Msg
    = DoThing String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoThing stringy ->
            ( model, Cmd.none )

-- main method
main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\m -> Sub.none)
        }