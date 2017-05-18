module Json
    exposing
        ( sessionDecoder
        , sessionEncoder
        , putSessionEncoder
        , mesEncoder
        , userEncoder
        )

import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional)
import Json.Encode as JE exposing (object)
import Game
import Time exposing (Time)
import String
import Model as M


sessionDecoder : Decoder Game.Session
sessionDecoder =
    decode Game.Session
        |> required "id" JD.string
        |> required "userId" JD.string
        |> required "gameId" JD.string
        |> required "start" stringToFloatDecoder
        |> optional "end" (JD.maybe JD.float) Nothing


stringToFloatDecoder : Decoder Float
stringToFloatDecoder =
    JD.string
        |> JD.andThen
            (\str ->
                case String.toFloat str of
                    Ok float ->
                        JD.succeed float

                    Err err ->
                        JD.fail err
            )


sessionEncoder : { a | userId : String, gameId : String, start : Time, end : Maybe Time } -> JE.Value
sessionEncoder { userId, gameId, start, end } =
    object
        [ ( "userId", userId |> JE.string )
        , ( "gameId", gameId |> JE.string )
        , ( "start", start |> toString |> JE.string )
        , ( "end", end |> Maybe.map (toString >> JE.string) |> Maybe.withDefault JE.null )
        ]


putSessionEncoder : Game.Session -> JE.Value
putSessionEncoder session =
    object
        [ ( "gsessionId", session.id |> JE.string )
        , ( "UserGsessionRecord", sessionEncoder session )
        ]


mesEncoder : M.MesAnswer -> String -> JE.Value
mesEncoder mes sub =
    JE.object
        [ ( "mesanswerReq"
          , JE.object
                [ ( "userId", JE.string sub )
                , ( "mesqueryId", JE.string mes.queryId )
                , ( "content", JE.string mes.essay )
                , ( "public", JE.bool mes.public )
                ]
          )
        ]


userEncoder : M.UserEdit -> JE.Value
userEncoder u_ =
    JE.object
        [ ( "userReq"
          , JE.object
                [ ( "username", JE.string u_.username )
                , ( "email", JE.string u_.email )
                , ( "firstName", JE.string u_.firstName )
                , ( "lastName", JE.string u_.lastName )
                , ( "groupId", JE.string u_.groupId )
                , ( "password", JE.string u_.password )
                ]
          )
        ]
