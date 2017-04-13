port module Port exposing (..)

import Json.Encode


type alias Key =
    String


type alias Value =
    Json.Encode.Value



{-
   From js
-}


port status : (String -> msg) -> Sub msg



{-
   To Js
-}
-- Task Server path, id of form, jwt token


port upload : ( String, String, String ) -> Cmd msg


port set : ( Key, Value ) -> Cmd msg


port remove : Key -> Cmd msg


port clear : () -> Cmd msg


port ping : () -> Cmd msg


port preload : List String -> Cmd msg
