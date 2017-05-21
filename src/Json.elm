module Json
    exposing
        ( sessionDecoder
        , sessionEncoder
        , cyclesEncoder
        , cycleEncoder
        , putSessionEncoder
        , mesEncoder
        , userEncoder
        , cycleDecoder
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
        |> optional "seed" stringToIntDecoder 0
        |> required "start" stringToFloatDecoder
        |> optional "end" (JD.maybe JD.float) Nothing


cycleDecoder : Decoder Game.Cycle
cycleDecoder =
    decode Game.Cycle
        |> required "id" (JD.maybe JD.string)
        |> required "gsessionId" JD.string
        |> optional "sort" JD.int 0
        |> optional "fixation" (stringToFloatDecoder |> JD.map numberToMaybe) Nothing
        |> optional "selection" (stringToFloatDecoder |> JD.map numberToMaybe) Nothing
        |> optional "pictures" (stringToFloatDecoder |> JD.map numberToMaybe) Nothing
        |> optional "redcross" (stringToFloatDecoder |> JD.map numberToMaybe) Nothing
        |> optional "probe" (stringToFloatDecoder |> JD.map numberToMaybe) Nothing
        |> optional "border" (stringToFloatDecoder |> JD.map numberToMaybe) Nothing
        |> optional "timeout" (stringToFloatDecoder |> JD.map numberToMaybe) Nothing
        |> optional "rest" (stringToFloatDecoder |> JD.map numberToMaybe) Nothing
        |> optional "width" (JD.int |> JD.map numberToMaybe) Nothing
        |> optional "height" (JD.int |> JD.map numberToMaybe) Nothing
        |> optional "blue" JD.bool False
        |> optional "grey" JD.bool False
        |> optional "dash" JD.bool False
        |> optional "probeIndex" (JD.int |> JD.map numberToMaybe) Nothing
        |> optional "targetIndex" JD.int 0
        |> optional "selectedIndex" JD.int 0
        |> optional "startIndex" JD.int 0
        |> optional "ugimageIds" (JD.list JD.string) []


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


stringToIntDecoder : Decoder Int
stringToIntDecoder =
    JD.string
        |> JD.andThen
            (\str ->
                case String.toInt str of
                    Ok int ->
                        JD.succeed int

                    Err err ->
                        JD.fail err
            )


sessionEncoder : { a | userId : String, gameId : String, start : Time, end : Maybe Time, seed : Int } -> JE.Value
sessionEncoder { userId, gameId, start, end, seed } =
    object
        [ ( "userId", userId |> JE.string )
        , ( "gameId", gameId |> JE.string )
        , ( "seed", seed |> toString |> JE.string )
        , ( "start", start |> toString |> JE.string )
        , ( "end", end |> Maybe.map (toString >> JE.string) |> Maybe.withDefault JE.null )
        ]


putSessionEncoder : Game.Session -> JE.Value
putSessionEncoder session =
    object
        [ ( "gsessionId", session.id |> JE.string )
        , ( "gsessionReq", sessionEncoder session )
        ]


cyclesEncoder : Game.Session -> List Game.Cycle -> JE.Value
cyclesEncoder session cycles =
    object
        [ ( "gsessionId", session.id |> JE.string )
        , ( "gcycles", cycles |> List.map cycleEncoder |> JE.list )
        ]


cycleEncoder : Game.Cycle -> JE.Value
cycleEncoder cycle =
    object
        [ ( "gsessionId", cycle.sessionId |> JE.string )
        , ( "sort", cycle.sort |> JE.int )
        , ( "fixation", cycle.fixation |> Maybe.withDefault 0 |> (toString >> JE.string) )
        , ( "selection", cycle.selection |> Maybe.withDefault 0 |> (toString >> JE.string) )
        , ( "pictures", cycle.pictures |> Maybe.withDefault 0 |> (toString >> JE.string) )
        , ( "redcross", cycle.redcross |> Maybe.withDefault 0 |> (toString >> JE.string) )
        , ( "probe", cycle.probe |> Maybe.withDefault 0 |> (toString >> JE.string) )
        , ( "border", cycle.border |> Maybe.withDefault 0 |> (toString >> JE.string) )
        , ( "timeout", cycle.timeout |> Maybe.withDefault 0 |> (toString >> JE.string) )
        , ( "width", cycle.width |> Maybe.withDefault 1 |> JE.int )
        , ( "height", cycle.height |> Maybe.withDefault 1 |> JE.int )
        , ( "blue", cycle.blue |> JE.bool )
        , ( "dash", cycle.dash |> JE.bool )
        , ( "probeIndex", cycle.probeIndex |> Maybe.withDefault 0 |> JE.int )
        , ( "targetIndex", cycle.targetIndex |> JE.int )
        , ( "selectedIndex", cycle.selectedIndex |> JE.int )
        , ( "startIndex", cycle.startIndex |> JE.int )
        , ( "ugimageIds", cycle.images |> List.map JE.string |> JE.list )
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

                -- , ( "password", JE.string u_.password )
                ]
          )
        ]



-- HELPERS


numberToMaybe : number -> Maybe number
numberToMaybe number =
    case number of
        0 ->
            Nothing

        _ ->
            Just number
