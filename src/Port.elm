port module Port exposing (..)

-- Ports to localStorage

import Entity
import Json.Encode


type alias Key =
    String


type alias Value =
    Json.Encode.Value


type alias CsvData =
    { contents : String
    , filename : String
    }


{-| Subscriptions (Receive from JS) & Commands (Send to JS)
-}



-- FileReader() access


port fileSelected : String -> Cmd msg


port fileContentRead : (CsvData -> msg) -> Sub msg



-- localStorage Access


port getUserResponse : (String -> msg) -> Sub msg


port storageGetItem : Key -> Cmd msg


port storageSetItem : ( Key, Value ) -> Cmd msg


port storageRemoveItem : Key -> Cmd msg


port storageClear : () -> Cmd msg



-- Play audio from page


port playAudioPing : () -> Cmd msg
