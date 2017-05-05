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
                        Game.BeginDisplay (Just (Game.Single _ _)) timestamp ->
                            ( Just timestamp, responseTimes )

                        Game.BeginDisplay (Just (Game.LeftRight _ _ _)) timestamp ->
                            ( Just timestamp, responseTimes )

                        Game.BeginDisplay (Just (Game.SelectGrid _ _ _ _)) timestamp ->
                            ( Just timestamp, responseTimes )

                        _ ->
                            ( Nothing, responseTimes )

                Just beginTime ->
                    case log of
                        Game.AcceptIndication _ responseTime ->
                            ( Nothing, (responseTime - beginTime) :: responseTimes )

                        Game.AcceptDirection _ _ responseTime ->
                            ( Nothing, (responseTime - beginTime) :: responseTimes )

                        Game.AcceptSelection _ _ responseTime ->
                            ( Nothing, (responseTime - beginTime) :: responseTimes )

                        Game.EndTrial _ ->
                            ( Nothing, responseTimes )

                        _ ->
                            ( Just beginTime, responseTimes )

        responseTimes =
            state.log
                |> Debug.log "log"
                |> List.foldr f ( Nothing, [] )
                |> Tuple.second
                |> Debug.log "responseTimes"

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
isCorrect gameEntry =
    case gameEntry of
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

        Game.PlaySound _ ->
            False

        Game.AcceptIndication noGoTrial _ ->
            noGoTrial

        Game.AcceptDirection expected answer _ ->
            expected == answer

        Game.AcceptSelection expected answer _ ->
            expected == answer

        Game.Timeout goTrial _ ->
            goTrial


isResult : Game.LogEntry -> Bool
isResult gameEntry =
    case gameEntry of
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

        Game.PlaySound _ ->
            False

        Game.AcceptIndication _ _ ->
            True

        Game.AcceptDirection _ _ _ ->
            True

        Game.AcceptSelection _ _ _ ->
            True

        Game.Timeout _ _ ->
            True
