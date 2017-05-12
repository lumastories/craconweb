module Json
    exposing
        ( sessionDecoder
        , sessionEncoder
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


sessionEncoder : { userId : String, gameId : String, start : Time, end : Maybe Time } -> JE.Value
sessionEncoder { userId, gameId, start, end } =
    object
        [ ( "userId", userId |> JE.string )
        , ( "gameId", gameId |> JE.string )
        , ( "start", start |> toString |> JE.string )
        , ( "end", end |> Maybe.map (toString >> JE.string) |> Maybe.withDefault JE.null )
        ]
