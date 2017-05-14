module Game.Result
    exposing
        ( filterResults
        , percentCorrect
        , averageResponseTimeInMillisecond
        )

import Game


filterResults : Game.State -> List Game.LogEntry
filterResults state =
    state.log
        |> List.filter isResult
        |> List.reverse


averageResponseTimeInMillisecond : Game.State -> Result String Float
averageResponseTimeInMillisecond state =
    let
        f log ( maybeStartTime, responseTimes ) =
            case maybeStartTime of
                Nothing ->
                    case log of
                        Game.BeginInput timestamp ->
                            ( Just timestamp, responseTimes )

                        _ ->
                            ( Nothing, responseTimes )

                Just beginTime ->
                    case log of
                        Game.AcceptIndication _ responseTime ->
                            ( Nothing, (responseTime - beginTime) :: responseTimes )

                        Game.AcceptDirection _ responseTime ->
                            ( Nothing, (responseTime - beginTime) :: responseTimes )

                        Game.AcceptSelection _ responseTime ->
                            ( Nothing, (responseTime - beginTime) :: responseTimes )

                        Game.EndTrial _ ->
                            ( Nothing, responseTimes )

                        _ ->
                            ( Just beginTime, responseTimes )

        responseTimes =
            state.log
                |> List.foldr f ( Nothing, [] )
                |> Tuple.second

        totalResponses =
            responseTimes |> List.length
    in
        if totalResponses == 0 then
            Err "No Response"
        else
            Ok <| List.sum responseTimes / toFloat totalResponses


percentCorrect : Game.State -> Float
percentCorrect state =
    let
        results =
            state.log |> List.filter isResult

        totalAnswer =
            List.length results

        correctAnswers =
            results |> List.filter isCorrect |> List.length
    in
        (toFloat correctAnswers / toFloat totalAnswer) * 100


isCorrect : Game.LogEntry -> Bool
isCorrect logEntry =
    case logEntry of
        Game.BeginSession _ _ ->
            False

        Game.EndSession _ ->
            False

        Game.BeginTrial _ ->
            False

        Game.EndTrial _ ->
            False

        Game.BeginDisplay _ _ ->
            False

        Game.BeginInput _ ->
            False

        Game.AcceptIndication { desired } _ ->
            desired

        Game.AcceptDirection { desired, actual } _ ->
            desired == actual

        Game.AcceptSelection { desired, actual } _ ->
            desired == actual

        Game.Timeout { desired } _ ->
            desired


isResult : Game.LogEntry -> Bool
isResult logEntry =
    case logEntry of
        Game.BeginSession _ _ ->
            False

        Game.EndSession _ ->
            False

        Game.BeginTrial _ ->
            False

        Game.EndTrial _ ->
            False

        Game.BeginDisplay _ _ ->
            False

        Game.BeginInput _ ->
            False

        Game.AcceptIndication _ _ ->
            True

        Game.AcceptDirection _ _ ->
            True

        Game.AcceptSelection _ _ ->
            True

        Game.Timeout _ _ ->
            True
