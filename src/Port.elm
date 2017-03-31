port module Port exposing (..)

-- Ports to localStorage


port removeItem : String -> Cmd msg


type alias KeyValue =
    ( String, String )


port setItem : KeyValue -> Cmd msg


port clearLocalStorage : Bool -> Cmd msg


port pinger : Bool -> Cmd msg
