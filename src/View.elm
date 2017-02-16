module View exposing (view)
import HtmlComponents as C
import Html exposing (..)
import Model exposing (..)


view : Model -> Html Msg
view model =
    C.rootView model