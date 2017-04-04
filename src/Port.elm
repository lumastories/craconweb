port module Port exposing (..)

import Json.Encode
import Api exposing (CsvData)


type alias Key =
    String


type alias Value =
    Json.Encode.Value


port fileSelected : String -> Cmd msg



-- cssId, userid, token


port uploadFile : ( String, String, String ) -> Cmd msg


port fileContentRead : (CsvData -> msg) -> Sub msg


port getUserResponse : (String -> msg) -> Sub msg


port storageGetItem : Key -> Cmd msg


port storageSetItem : ( Key, Value ) -> Cmd msg


port storageRemoveItem : Key -> Cmd msg


port storageClear : () -> Cmd msg


port playAudioPing : () -> Cmd msg
