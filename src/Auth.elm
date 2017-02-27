module Auth exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline as JP
import Json.Encode as Encode


type alias JwtPayload =
    { aud : String
    , exp : Int
    , iat : Int
    , iss : String
    , sub : String
    , roles : List Role
    }


type alias Role =
    { id : String
    , name : String
    , weight : Int
    }


decodeJwtPayload : Decode.Decoder JwtPayload
decodeJwtPayload =
    JP.decode JwtPayload
        |> JP.required "aud" (Decode.string)
        |> JP.required "exp" (Decode.int)
        |> JP.required "iat" (Decode.int)
        |> JP.required "iss" (Decode.string)
        |> JP.required "sub" (Decode.string)
        |> JP.required "roles" (Decode.list decodeRole)


decodeRole : Decode.Decoder Role
decodeRole =
    JP.decode Role
        |> JP.required "id" (Decode.string)
        |> JP.required "name" (Decode.string)
        |> JP.required "weight" (Decode.int)
