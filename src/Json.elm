module Json
    exposing
        ( sessionDecoder
        , sessionEncoder
        , putSessionEncoder
        )

import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional)
import Json.Encode as JE exposing (object)
import Game
import Time exposing (Time)
import String


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
