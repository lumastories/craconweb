module Helpers exposing (..)

import Http
import Model exposing (errorCodeEncoder)


httpHumanError : Http.Error -> String
httpHumanError err =
    case err of
        Http.Timeout ->
            "Something is taking too long"

        Http.NetworkError ->
            "Oops. There's been a network error."

        Http.BadStatus s ->
            "Server error: " ++ (.error (errorCodeEncoder s.body))

        Http.BadPayload str _ ->
            "Bad payload"

        _ ->
            "Unknown error"
