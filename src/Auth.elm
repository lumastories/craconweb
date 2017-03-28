module Auth exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline as JP
import Entity
import Jwt


jwtDecoded token =
    Jwt.decodeToken decodeJwtPayload token


type alias JwtPayload =
    { aud : String
    , exp : Int
    , iat : Int
    , iss : String
    , sub : String
    , roles : List Entity.Role
    }


decodeJwtPayload : Decode.Decoder JwtPayload
decodeJwtPayload =
    JP.decode JwtPayload
        |> JP.required "aud" (Decode.string)
        |> JP.required "exp" (Decode.int)
        |> JP.required "iat" (Decode.int)
        |> JP.required "iss" (Decode.string)
        |> JP.required "sub" (Decode.string)
        |> JP.required "roles" (Decode.list Entity.roleDecoder)
