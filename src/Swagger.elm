module Decoder exposing (..)

import Json.Decode exposing (Decoder, string, int, float, dict, list, bool, map, value, decodeValue, decodeString)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)


maybe : String -> Decoder a -> Decoder (Maybe a -> b) -> Decoder b
maybe name decoder =
    optional name (map Just decoder) Nothing


lazy : (() -> Decoder a) -> Decoder a
lazy thunk =
    customDecoder value
        (\js -> decodeValue (thunk ()) js)


customDecoder : Decoder a -> (a -> Result String b) -> Decoder b
customDecoder decoder toResult =
    Json.Decode.andThen
        (\a ->
            case toResult a of
                Ok b ->
                    Json.Decode.succeed b

                Err err ->
                    Json.Decode.fail err
        )
        decoder


type alias IdlAuth =
    { token : Maybe String
    }


type alias IdlAuthRecord =
    { email : Maybe String
    , password : Maybe String
    }


type alias IdlAuthRedactor =
    { token : Maybe String
    }


type alias IdlEmpty =
    {}


type alias IdlGame =
    { blockCt : Maybe Int
    , blocked : Maybe String
    , created : Maybe String
    , decTrigger : Maybe Int
    , deleted : Maybe String
    , description : Maybe String
    , durDec : Maybe Int
    , durInc : Maybe Int
    , duration : Maybe Int
    , fixImagePath : Maybe String
    , icon : Maybe String
    , id : Maybe String
    , incTrigger : Maybe Int
    , interval : Maybe Int
    , name : Maybe String
    , offset : Maybe Int
    , slug : Maybe String
    , trialCt : Maybe Int
    , updated : Maybe String
    }


type alias IdlGameRequest =
    { slug : Maybe String
    }


type alias IdlGameplay =
    { created : Maybe String
    , duration : Maybe Int
    , gameId : Maybe String
    , id : Maybe String
    , updated : Maybe String
    }


type alias IdlGameplays =
    { gameplays : Maybe (List IdlGameplaysGameplays)
    }


type alias IdlGameplaysReferral =
    { gameIds : Maybe (List IdlGameplaysReferralGameIds)
    , userId : Maybe String
    }


type alias IdlGimage =
    { created : Maybe String
    , deleted : Maybe String
    , groupId : Maybe String
    , id : Maybe String
    , path : Maybe String
    , updated : Maybe String
    , valid : Maybe Bool
    }


type alias IdlGimages =
    { gimages : Maybe (List IdlGimagesGimages)
    }


type alias IdlGimagesRange =
    { groupId : Maybe String
    , limit : Maybe Int
    , valid : Maybe Bool
    }


type alias IdlRole =
    { id : Maybe String
    , name : Maybe String
    , weight : Maybe Int
    }


type alias IdlUser =
    { avatar : Maybe String
    , blocked : Maybe String
    , created : Maybe String
    , deleted : Maybe String
    , email : Maybe String
    , firstName : Maybe String
    , groupID : Maybe String
    , id : Maybe String
    , lastLogin : Maybe String
    , lastName : Maybe String
    , roles : Maybe (List IdlUserRoles)
    , updated : Maybe String
    , username : Maybe String
    }


type alias IdlUserPassReset =
    {}


type alias IdlUserPassResetRecord =
    { email : Maybe String
    , proof : Maybe String
    }


type alias IdlUserRecord =
    { avatar : Maybe String
    , email : Maybe String
    , firstName : Maybe String
    , groupID : Maybe String
    , lastName : Maybe String
    , password : Maybe String
    , roles : Maybe (List IdlUserRecordRoles)
    , username : Maybe String
    }


type alias IdlUserRequest =
    { id : Maybe String
    }


type alias IdlUsers =
    { users : Maybe (List IdlUsersUsers)
    }


type alias IdlUsersRecord =
    { records : Maybe (List IdlUsersRecordRecords)
    }


type alias IdlUsersRequest =
    { ids : Maybe (List IdlUsersRequestIds)
    }


decodeIdlAuth : Decoder IdlAuth
decodeIdlAuth =
    decode idlAuth
        |> maybe "token" string


decodeIdlAuthRecord : Decoder IdlAuthRecord
decodeIdlAuthRecord =
    decode idlAuthRecord
        |> maybe "email" string
        |> maybe "password" string


decodeIdlAuthRedactor : Decoder IdlAuthRedactor
decodeIdlAuthRedactor =
    decode idlAuthRedactor
        |> maybe "token" string


decodeIdlEmpty : Decoder IdlEmpty
decodeIdlEmpty =
    decode idlEmpty


decodeIdlGame : Decoder IdlGame
decodeIdlGame =
    decode idlGame
        |> maybe "blockCt" int
        |> maybe "blocked" string
        |> maybe "created" string
        |> maybe "decTrigger" int
        |> maybe "deleted" string
        |> maybe "description" string
        |> maybe "durDec" int
        |> maybe "durInc" int
        |> maybe "duration" int
        |> maybe "fixImagePath" string
        |> maybe "icon" string
        |> maybe "id" string
        |> maybe "incTrigger" int
        |> maybe "interval" int
        |> maybe "name" string
        |> maybe "offset" int
        |> maybe "slug" string
        |> maybe "trialCt" int
        |> maybe "updated" string


decodeIdlGameRequest : Decoder IdlGameRequest
decodeIdlGameRequest =
    decode idlGameRequest
        |> maybe "slug" string


decodeIdlGameplay : Decoder IdlGameplay
decodeIdlGameplay =
    decode idlGameplay
        |> maybe "created" string
        |> maybe "duration" int
        |> maybe "gameId" string
        |> maybe "id" string
        |> maybe "updated" string


decodeIdlGameplays : Decoder IdlGameplays
decodeIdlGameplays =
    decode idlGameplays
        |> maybe "gameplays" (list (decodeIdlGameplaysGameplays))


decodeIdlGameplaysReferral : Decoder IdlGameplaysReferral
decodeIdlGameplaysReferral =
    decode idlGameplaysReferral
        |> maybe "gameIds" (list (decodeIdlGameplaysReferralGameIds))
        |> maybe "userId" string


decodeIdlGimage : Decoder IdlGimage
decodeIdlGimage =
    decode idlGimage
        |> maybe "created" string
        |> maybe "deleted" string
        |> maybe "groupId" string
        |> maybe "id" string
        |> maybe "path" string
        |> maybe "updated" string
        |> maybe "valid" bool


decodeIdlGimages : Decoder IdlGimages
decodeIdlGimages =
    decode idlGimages
        |> maybe "gimages" (list (decodeIdlGimagesGimages))


decodeIdlGimagesRange : Decoder IdlGimagesRange
decodeIdlGimagesRange =
    decode idlGimagesRange
        |> maybe "groupId" string
        |> maybe "limit" int
        |> maybe "valid" bool


decodeIdlRole : Decoder IdlRole
decodeIdlRole =
    decode idlRole
        |> maybe "id" string
        |> maybe "name" string
        |> maybe "weight" int


decodeIdlUser : Decoder IdlUser
decodeIdlUser =
    decode idlUser
        |> maybe "avatar" string
        |> maybe "blocked" string
        |> maybe "created" string
        |> maybe "deleted" string
        |> maybe "email" string
        |> maybe "firstName" string
        |> maybe "groupID" string
        |> maybe "id" string
        |> maybe "lastLogin" string
        |> maybe "lastName" string
        |> maybe "roles" (list (decodeIdlUserRoles))
        |> maybe "updated" string
        |> maybe "username" string


decodeIdlUserPassReset : Decoder IdlUserPassReset
decodeIdlUserPassReset =
    decode idlUserPassReset


decodeIdlUserPassResetRecord : Decoder IdlUserPassResetRecord
decodeIdlUserPassResetRecord =
    decode idlUserPassResetRecord
        |> maybe "email" string
        |> maybe "proof" string


decodeIdlUserRecord : Decoder IdlUserRecord
decodeIdlUserRecord =
    decode idlUserRecord
        |> maybe "avatar" string
        |> maybe "email" string
        |> maybe "firstName" string
        |> maybe "groupID" string
        |> maybe "lastName" string
        |> maybe "password" string
        |> maybe "roles" (list (decodeIdlUserRecordRoles))
        |> maybe "username" string


decodeIdlUserRequest : Decoder IdlUserRequest
decodeIdlUserRequest =
    decode idlUserRequest
        |> maybe "id" string


decodeIdlUsers : Decoder IdlUsers
decodeIdlUsers =
    decode idlUsers
        |> maybe "users" (list (decodeIdlUsersUsers))


decodeIdlUsersRecord : Decoder IdlUsersRecord
decodeIdlUsersRecord =
    decode idlUsersRecord
        |> maybe "records" (list (decodeIdlUsersRecordRecords))


decodeIdlUsersRequest : Decoder IdlUsersRequest
decodeIdlUsersRequest =
    decode idlUsersRequest
        |> maybe "ids" (list (decodeIdlUsersRequestIds))
