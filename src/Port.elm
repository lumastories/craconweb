port module Port exposing (..)

-- Ports to localStorage

import Entity
import Json.Encode


type alias Key =
    String


type alias Value =
    Json.Encode.Value


{-| Subscriptions (Receive from JS)
-}



--port storageGetItemResponse : (( Key, Value ) -> msg) -> Sub msg


port getUserResponse : (String -> msg) -> Sub msg


{-| Commands (Send to JS)
-}
port storageGetItem : Key -> Cmd msg


port storageSetItem : ( Key, Value ) -> Cmd msg


port storageRemoveItem : Key -> Cmd msg


port storageClear : () -> Cmd msg



-- Play audio from page


port playAudioPing : () -> Cmd msg
