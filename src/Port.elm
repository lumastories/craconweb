port module Port exposing (..)

-- Ports to localStorage


port removeItem : String -> Cmd msg


type alias KeyValue =
    ( String, String )


port setItem : KeyValue -> Cmd msg
