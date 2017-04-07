port module Port exposing (..)

import Json.Encode


type alias Key =
    String


type alias Value =
    Json.Encode.Value


port uploadFile : ( String, String ) -> Cmd msg


port status : (String -> msg) -> Sub msg


port getUserResponse : (String -> msg) -> Sub msg


port storageGetItem : Key -> Cmd msg


port storageSetItem : ( Key, Value ) -> Cmd msg


port storageRemoveItem : Key -> Cmd msg


port storageClear : () -> Cmd msg


port playAudioPing : () -> Cmd msg
