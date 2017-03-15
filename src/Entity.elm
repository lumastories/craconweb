module Entity exposing (..)

import Protobuf exposing (..)
import Json.Decode as JD
import Json.Encode as JE


type alias Group =
    { id : Int
    , name : String
    , slug : String
    , created : Maybe Timestamp
    , updated : Maybe Timestamp
    }


groupDecoder : JD.Decoder Group
groupDecoder =
    JD.lazy <|
        \_ ->
            decode Group
                |> required "id" JD.int 0
                |> required "name" JD.string ""
                |> required "slug" JD.string ""
                |> optional "created" timestampDecoder
                |> optional "updated" timestampDecoder


groupEncoder : Group -> JE.Value
groupEncoder v =
    JE.object <|
        List.filterMap identity <|
            [ (requiredFieldEncoder "id" JE.int 0 v.id)
            , (requiredFieldEncoder "name" JE.string "" v.name)
            , (requiredFieldEncoder "slug" JE.string "" v.slug)
            , (optionalEncoder "created" timestampEncoder v.created)
            , (optionalEncoder "updated" timestampEncoder v.updated)
            ]


type alias Role =
    { id : Int
    , name : String
    , weight : Int
    }


roleDecoder : JD.Decoder Role
roleDecoder =
    JD.lazy <|
        \_ ->
            decode Role
                |> required "id" JD.int 0
                |> required "name" JD.string ""
                |> required "weight" JD.int 0


roleEncoder : Role -> JE.Value
roleEncoder v =
    JE.object <|
        List.filterMap identity <|
            [ (requiredFieldEncoder "id" JE.int 0 v.id)
            , (requiredFieldEncoder "name" JE.string "" v.name)
            , (requiredFieldEncoder "weight" JE.int 0 v.weight)
            ]


type alias User =
    { id : Int
    , username : String
    , email : String
    , firstName : String
    , lastName : String
    , avatar : String
    , groupID : Int
    , roles : List Role
    , lastLogin : Maybe Timestamp
    , blocked : Maybe Timestamp
    , created : Maybe Timestamp
    , updated : Maybe Timestamp
    , deleted : Maybe Timestamp
    }


userDecoder : JD.Decoder User
userDecoder =
    JD.lazy <|
        \_ ->
            decode User
                |> required "id" JD.int 0
                |> required "username" JD.string ""
                |> required "email" JD.string ""
                |> required "firstName" JD.string ""
                |> required "lastName" JD.string ""
                |> required "avatar" JD.string ""
                |> required "groupID" JD.int 0
                |> repeated "roles" roleDecoder
                |> optional "lastLogin" timestampDecoder
                |> optional "blocked" timestampDecoder
                |> optional "created" timestampDecoder
                |> optional "updated" timestampDecoder
                |> optional "deleted" timestampDecoder


userEncoder : User -> JE.Value
userEncoder v =
    JE.object <|
        List.filterMap identity <|
            [ (requiredFieldEncoder "id" JE.int 0 v.id)
            , (requiredFieldEncoder "username" JE.string "" v.username)
            , (requiredFieldEncoder "email" JE.string "" v.email)
            , (requiredFieldEncoder "firstName" JE.string "" v.firstName)
            , (requiredFieldEncoder "lastName" JE.string "" v.lastName)
            , (requiredFieldEncoder "avatar" JE.string "" v.avatar)
            , (requiredFieldEncoder "groupID" JE.int 0 v.groupID)
            , (repeatedFieldEncoder "roles" roleEncoder v.roles)
            , (optionalEncoder "lastLogin" timestampEncoder v.lastLogin)
            , (optionalEncoder "blocked" timestampEncoder v.blocked)
            , (optionalEncoder "created" timestampEncoder v.created)
            , (optionalEncoder "updated" timestampEncoder v.updated)
            , (optionalEncoder "deleted" timestampEncoder v.deleted)
            ]


type alias UserRecord =
    { username :
        String
    , email :
        String
    , firstName :
        String
    , lastName :
        String
    , avatar :
        String
    , groupID :
        Int
    , roles :
        List Int
    , password :
        String
    }


userRecordDecoder : JD.Decoder UserRecord
userRecordDecoder =
    JD.lazy <|
        \_ ->
            decode UserRecord
                |> required "username" JD.string ""
                |> required "email" JD.string ""
                |> required "firstName" JD.string ""
                |> required "lastName" JD.string ""
                |> required "avatar" JD.string ""
                |> required "groupID" JD.int 0
                |> repeated "roles" JD.int
                |> required "password" JD.string ""


userRecordEncoder : UserRecord -> JE.Value
userRecordEncoder v =
    JE.object <|
        List.filterMap identity <|
            [ (requiredFieldEncoder "username" JE.string "" v.username)
            , (requiredFieldEncoder "email" JE.string "" v.email)
            , (requiredFieldEncoder "firstName" JE.string "" v.firstName)
            , (requiredFieldEncoder "lastName" JE.string "" v.lastName)
            , (requiredFieldEncoder "avatar" JE.string "" v.avatar)
            , (requiredFieldEncoder "groupID" JE.int 0 v.groupID)
            , (repeatedFieldEncoder "roles" JE.int v.roles)
            , (requiredFieldEncoder "password" JE.string "" v.password)
            ]


type alias UserRequest =
    { id :
        Int
    }


userRequestDecoder : JD.Decoder UserRequest
userRequestDecoder =
    JD.lazy <|
        \_ ->
            decode UserRequest
                |> required "id" JD.int 0


userRequestEncoder : UserRequest -> JE.Value
userRequestEncoder v =
    JE.object <|
        List.filterMap identity <|
            [ (requiredFieldEncoder "id" JE.int 0 v.id)
            ]


type alias UserPassResetRecord =
    { email :
        String
    , proof :
        String
    }


userPassResetRecordDecoder : JD.Decoder UserPassResetRecord
userPassResetRecordDecoder =
    JD.lazy <|
        \_ ->
            decode UserPassResetRecord
                |> required "email" JD.string ""
                |> required "proof" JD.string ""


userPassResetRecordEncoder : UserPassResetRecord -> JE.Value
userPassResetRecordEncoder v =
    JE.object <|
        List.filterMap identity <|
            [ (requiredFieldEncoder "email" JE.string "" v.email)
            , (requiredFieldEncoder "proof" JE.string "" v.proof)
            ]


type alias Users =
    { users :
        List User
    }


usersDecoder : JD.Decoder Users
usersDecoder =
    JD.lazy <|
        \_ ->
            decode Users
                |> repeated "users" userDecoder


usersEncoder : Users -> JE.Value
usersEncoder v =
    JE.object <|
        List.filterMap identity <|
            [ (repeatedFieldEncoder "users" userEncoder v.users)
            ]


type alias UsersRecord =
    { records :
        List UserRecord
    }


usersRecordDecoder : JD.Decoder UsersRecord
usersRecordDecoder =
    JD.lazy <|
        \_ ->
            decode UsersRecord
                |> repeated "records" userRecordDecoder


usersRecordEncoder : UsersRecord -> JE.Value
usersRecordEncoder v =
    JE.object <|
        List.filterMap identity <|
            [ (repeatedFieldEncoder "records" userRecordEncoder v.records)
            ]


type alias UsersRequest =
    { ids :
        List Int
    }


usersRequestDecoder : JD.Decoder UsersRequest
usersRequestDecoder =
    JD.lazy <|
        \_ ->
            decode UsersRequest
                |> repeated "ids" JD.int


usersRequestEncoder : UsersRequest -> JE.Value
usersRequestEncoder v =
    JE.object <|
        List.filterMap identity <|
            [ (repeatedFieldEncoder "ids" JE.int v.ids)
            ]


type alias UsersReferral =
    { idFirst :
        Int
    , idFinal :
        Int
    , idDesc :
        Bool
    , limit :
        Int
    , skip :
        Int
    }


usersReferralDecoder : JD.Decoder UsersReferral
usersReferralDecoder =
    JD.lazy <|
        \_ ->
            decode UsersReferral
                |> required "idFirst" JD.int 0
                |> required "idFinal" JD.int 0
                |> required "idDesc" JD.bool False
                |> required "limit" JD.int 0
                |> required "skip" JD.int 0


usersReferralEncoder : UsersReferral -> JE.Value
usersReferralEncoder v =
    JE.object <|
        List.filterMap identity <|
            [ (requiredFieldEncoder "idFirst" JE.int 0 v.idFirst)
            , (requiredFieldEncoder "idFinal" JE.int 0 v.idFinal)
            , (requiredFieldEncoder "idDesc" JE.bool False v.idDesc)
            , (requiredFieldEncoder "limit" JE.int 0 v.limit)
            , (requiredFieldEncoder "skip" JE.int 0 v.skip)
            ]


type alias Auth =
    { token :
        String
    }


authDecoder : JD.Decoder Auth
authDecoder =
    JD.lazy <|
        \_ ->
            decode Auth
                |> required "token" JD.string ""


authEncoder : Auth -> JE.Value
authEncoder v =
    JE.object <|
        List.filterMap identity <|
            [ (requiredFieldEncoder "token" JE.string "" v.token)
            ]


type alias AuthRecord =
    { email : String
    , password : String
    }


authRecordDecoder : JD.Decoder AuthRecord
authRecordDecoder =
    JD.lazy <|
        \_ ->
            decode AuthRecord
                |> required "email" JD.string ""
                |> required "password" JD.string ""


authRecordEncoder : AuthRecord -> JE.Value
authRecordEncoder v =
    JE.object <|
        List.filterMap identity <|
            [ (requiredFieldEncoder "email" JE.string "" v.email)
            , (requiredFieldEncoder "password" JE.string "" v.password)
            ]


type alias AuthRedactor =
    { token :
        String
    }


authRedactorDecoder : JD.Decoder AuthRedactor
authRedactorDecoder =
    JD.lazy <|
        \_ ->
            decode AuthRedactor
                |> required "token" JD.string ""


authRedactorEncoder : AuthRedactor -> JE.Value
authRedactorEncoder v =
    JE.object <|
        List.filterMap identity <|
            [ (requiredFieldEncoder "token" JE.string "" v.token)
            ]


type alias Game =
    { id : Int
    , name : String
    , slug : String
    , description : String
    , icon : String
    , trialCt : Int
    , blockCt : Int
    , duration : Int
    , offset : Int
    , interval : Int
    , fixImagePath : String
    , durInc : Int
    , durDec : Int
    , incTrigger : Int
    , decTrigger : Int
    , blocked : Maybe Timestamp
    , created : Maybe Timestamp
    , updated : Maybe Timestamp
    , deleted : Maybe Timestamp
    }


gameDecoder : JD.Decoder Game
gameDecoder =
    JD.lazy <|
        \_ ->
            decode Game
                |> required "id" JD.int 0
                |> required "name" JD.string ""
                |> required "slug" JD.string ""
                |> required "description" JD.string ""
                |> required "icon" JD.string ""
                |> required "trialCt" JD.int 0
                |> required "blockCt" JD.int 0
                |> required "duration" JD.int 0
                |> required "offset" JD.int 0
                |> required "interval" JD.int 0
                |> required "fixImagePath" JD.string ""
                |> required "durInc" JD.int 0
                |> required "durDec" JD.int 0
                |> required "incTrigger" JD.int 0
                |> required "decTrigger" JD.int 0
                |> optional "blocked" timestampDecoder
                |> optional "created" timestampDecoder
                |> optional "updated" timestampDecoder
                |> optional "deleted" timestampDecoder


gameEncoder : Game -> JE.Value
gameEncoder v =
    JE.object <|
        List.filterMap identity <|
            [ (requiredFieldEncoder "id" JE.int 0 v.id)
            , (requiredFieldEncoder "name" JE.string "" v.name)
            , (requiredFieldEncoder "slug" JE.string "" v.slug)
            , (requiredFieldEncoder "description" JE.string "" v.description)
            , (requiredFieldEncoder "icon" JE.string "" v.icon)
            , (requiredFieldEncoder "trialCt" JE.int 0 v.trialCt)
            , (requiredFieldEncoder "blockCt" JE.int 0 v.blockCt)
            , (requiredFieldEncoder "duration" JE.int 0 v.duration)
            , (requiredFieldEncoder "offset" JE.int 0 v.offset)
            , (requiredFieldEncoder "interval" JE.int 0 v.interval)
            , (requiredFieldEncoder "fixImagePath" JE.string "" v.fixImagePath)
            , (requiredFieldEncoder "durInc" JE.int 0 v.durInc)
            , (requiredFieldEncoder "durDec" JE.int 0 v.durDec)
            , (requiredFieldEncoder "incTrigger" JE.int 0 v.incTrigger)
            , (requiredFieldEncoder "decTrigger" JE.int 0 v.decTrigger)
            , (optionalEncoder "blocked" timestampEncoder v.blocked)
            , (optionalEncoder "created" timestampEncoder v.created)
            , (optionalEncoder "updated" timestampEncoder v.updated)
            , (optionalEncoder "deleted" timestampEncoder v.deleted)
            ]


type alias GameRequest =
    { slug :
        String
    }


gameRequestDecoder : JD.Decoder GameRequest
gameRequestDecoder =
    JD.lazy <|
        \_ ->
            decode GameRequest
                |> required "slug" JD.string ""


gameRequestEncoder : GameRequest -> JE.Value
gameRequestEncoder v =
    JE.object <|
        List.filterMap identity <|
            [ (requiredFieldEncoder "slug" JE.string "" v.slug)
            ]


type alias Gameplay =
    { id : Int
    , gameId : Int
    , duration : Int
    , created : Maybe Timestamp
    , updated : Maybe Timestamp
    }


gameplayDecoder : JD.Decoder Gameplay
gameplayDecoder =
    JD.lazy <|
        \_ ->
            decode Gameplay
                |> required "id" JD.int 0
                |> required "gameId" JD.int 0
                |> required "duration" JD.int 0
                |> optional "created" timestampDecoder
                |> optional "updated" timestampDecoder


gameplayEncoder : Gameplay -> JE.Value
gameplayEncoder v =
    JE.object <|
        List.filterMap identity <|
            [ (requiredFieldEncoder "id" JE.int 0 v.id)
            , (requiredFieldEncoder "gameId" JE.int 0 v.gameId)
            , (requiredFieldEncoder "duration" JE.int 0 v.duration)
            , (optionalEncoder "created" timestampEncoder v.created)
            , (optionalEncoder "updated" timestampEncoder v.updated)
            ]


type alias Gameplays =
    { gameplays :
        List Gameplay
    }


gameplaysDecoder : JD.Decoder Gameplays
gameplaysDecoder =
    JD.lazy <|
        \_ ->
            decode Gameplays
                |> repeated "gameplays" gameplayDecoder


gameplaysEncoder : Gameplays -> JE.Value
gameplaysEncoder v =
    JE.object <|
        List.filterMap identity <|
            [ (repeatedFieldEncoder "gameplays" gameplayEncoder v.gameplays)
            ]


type alias UserGameplaysReferral =
    { userId :
        Int
    , gameIds :
        List Int
    }


userGameplaysReferralDecoder : JD.Decoder UserGameplaysReferral
userGameplaysReferralDecoder =
    JD.lazy <|
        \_ ->
            decode UserGameplaysReferral
                |> required "userId" JD.int 0
                |> repeated "gameIds" JD.int


userGameplaysReferralEncoder : UserGameplaysReferral -> JE.Value
userGameplaysReferralEncoder v =
    JE.object <|
        List.filterMap identity <|
            [ (requiredFieldEncoder "userId" JE.int 0 v.userId)
            , (repeatedFieldEncoder "gameIds" JE.int v.gameIds)
            ]


type alias Gimgtype =
    { id :
        Int
    , slug :
        String
    }


gimgtypeDecoder : JD.Decoder Gimgtype
gimgtypeDecoder =
    JD.lazy <|
        \_ ->
            decode Gimgtype
                |> required "id" JD.int 0
                |> required "slug" JD.string ""


gimgtypeEncoder : Gimgtype -> JE.Value
gimgtypeEncoder v =
    JE.object <|
        List.filterMap identity <|
            [ (requiredFieldEncoder "id" JE.int 0 v.id)
            , (requiredFieldEncoder "slug" JE.string "" v.slug)
            ]


type alias Gimage =
    { id : Int
    , name : String
    , path : String
    , gimgtypeId : Int
    , created : Maybe Timestamp
    , updated : Maybe Timestamp
    , deleted : Maybe Timestamp
    }


gimageDecoder : JD.Decoder Gimage
gimageDecoder =
    JD.lazy <|
        \_ ->
            decode Gimage
                |> required "id" JD.int 0
                |> required "name" JD.string ""
                |> required "path" JD.string ""
                |> required "gimgtypeId" JD.int 0
                |> optional "created" timestampDecoder
                |> optional "updated" timestampDecoder
                |> optional "deleted" timestampDecoder


gimageEncoder : Gimage -> JE.Value
gimageEncoder v =
    JE.object <|
        List.filterMap identity <|
            [ (requiredFieldEncoder "id" JE.int 0 v.id)
            , (requiredFieldEncoder "name" JE.string "" v.name)
            , (requiredFieldEncoder "path" JE.string "" v.path)
            , (requiredFieldEncoder "gimgtypeId" JE.int 0 v.gimgtypeId)
            , (optionalEncoder "created" timestampEncoder v.created)
            , (optionalEncoder "updated" timestampEncoder v.updated)
            , (optionalEncoder "deleted" timestampEncoder v.deleted)
            ]


type alias Ugimgset =
    { id :
        Int
    , userId :
        Int
    , created :
        Maybe Timestamp
    , updated :
        Maybe Timestamp
    }


ugimgsetDecoder : JD.Decoder Ugimgset
ugimgsetDecoder =
    JD.lazy <|
        \_ ->
            decode Ugimgset
                |> required "id" JD.int 0
                |> required "userId" JD.int 0
                |> optional "created" timestampDecoder
                |> optional "updated" timestampDecoder


ugimgsetEncoder : Ugimgset -> JE.Value
ugimgsetEncoder v =
    JE.object <|
        List.filterMap identity <|
            [ (requiredFieldEncoder "id" JE.int 0 v.id)
            , (requiredFieldEncoder "userId" JE.int 0 v.userId)
            , (optionalEncoder "created" timestampEncoder v.created)
            , (optionalEncoder "updated" timestampEncoder v.updated)
            ]


type alias Ugimgsets =
    { ugimgsets :
        List Ugimgset
    }


ugimgsetsDecoder : JD.Decoder Ugimgsets
ugimgsetsDecoder =
    JD.lazy <|
        \_ ->
            decode Ugimgsets
                |> repeated "ugimgsets" ugimgsetDecoder


ugimgsetsEncoder : Ugimgsets -> JE.Value
ugimgsetsEncoder v =
    JE.object <|
        List.filterMap identity <|
            [ (repeatedFieldEncoder "ugimgsets" ugimgsetEncoder v.ugimgsets)
            ]


type alias UserUgimgsetsReferral =
    { userId :
        Int
    , createdFirst :
        Maybe Timestamp
    , createdFinal :
        Maybe Timestamp
    , createdDesc :
        Bool
    , limit :
        Int
    , skip :
        Int
    }


userUgimgsetsReferralDecoder : JD.Decoder UserUgimgsetsReferral
userUgimgsetsReferralDecoder =
    JD.lazy <|
        \_ ->
            decode UserUgimgsetsReferral
                |> required "userId" JD.int 0
                |> optional "createdFirst" timestampDecoder
                |> optional "createdFinal" timestampDecoder
                |> required "createdDesc" JD.bool False
                |> required "limit" JD.int 0
                |> required "skip" JD.int 0


userUgimgsetsReferralEncoder : UserUgimgsetsReferral -> JE.Value
userUgimgsetsReferralEncoder v =
    JE.object <|
        List.filterMap identity <|
            [ (requiredFieldEncoder "userId" JE.int 0 v.userId)
            , (optionalEncoder "createdFirst" timestampEncoder v.createdFirst)
            , (optionalEncoder "createdFinal" timestampEncoder v.createdFinal)
            , (requiredFieldEncoder "createdDesc" JE.bool False v.createdDesc)
            , (requiredFieldEncoder "limit" JE.int 0 v.limit)
            , (requiredFieldEncoder "skip" JE.int 0 v.skip)
            ]


type alias Ugimage =
    { id :
        Int
    , gimage :
        Maybe Gimage
    , val :
        Int
    }


ugimageDecoder : JD.Decoder Ugimage
ugimageDecoder =
    JD.lazy <|
        \_ ->
            decode Ugimage
                |> required "id" JD.int 0
                |> optional "gimage" gimageDecoder
                |> required "val" JD.int 0


ugimageEncoder : Ugimage -> JE.Value
ugimageEncoder v =
    JE.object <|
        List.filterMap identity <|
            [ (requiredFieldEncoder "id" JE.int 0 v.id)
            , (optionalEncoder "gimage" gimageEncoder v.gimage)
            , (requiredFieldEncoder "val" JE.int 0 v.val)
            ]


type alias Ugimages =
    { ugimages :
        List Ugimage
    }


ugimagesDecoder : JD.Decoder Ugimages
ugimagesDecoder =
    JD.lazy <|
        \_ ->
            decode Ugimages
                |> repeated "ugimages" ugimageDecoder


ugimagesEncoder : Ugimages -> JE.Value
ugimagesEncoder v =
    JE.object <|
        List.filterMap identity <|
            [ (repeatedFieldEncoder "ugimages" ugimageEncoder v.ugimages)
            ]


type alias UgimgsetUgimagesReferral =
    { ugimgsetId :
        Int
    , gimgtypeSlug :
        String
    , valFirst :
        Int
    , valFinal :
        Int
    , valDesc :
        Bool
    , limit :
        Int
    , skip :
        Int
    }


ugimgsetUgimagesReferralDecoder : JD.Decoder UgimgsetUgimagesReferral
ugimgsetUgimagesReferralDecoder =
    JD.lazy <|
        \_ ->
            decode UgimgsetUgimagesReferral
                |> required "ugimgsetId" JD.int 0
                |> required "gimgtypeSlug" JD.string ""
                |> required "valFirst" JD.int 0
                |> required "valFinal" JD.int 0
                |> required "valDesc" JD.bool False
                |> required "limit" JD.int 0
                |> required "skip" JD.int 0


ugimgsetUgimagesReferralEncoder : UgimgsetUgimagesReferral -> JE.Value
ugimgsetUgimagesReferralEncoder v =
    JE.object <|
        List.filterMap identity <|
            [ (requiredFieldEncoder "ugimgsetId" JE.int 0 v.ugimgsetId)
            , (requiredFieldEncoder "gimgtypeSlug" JE.string "" v.gimgtypeSlug)
            , (requiredFieldEncoder "valFirst" JE.int 0 v.valFirst)
            , (requiredFieldEncoder "valFinal" JE.int 0 v.valFinal)
            , (requiredFieldEncoder "valDesc" JE.bool False v.valDesc)
            , (requiredFieldEncoder "limit" JE.int 0 v.limit)
            , (requiredFieldEncoder "skip" JE.int 0 v.skip)
            ]
