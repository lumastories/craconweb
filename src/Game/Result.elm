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


averageResponseTimeInMillisecond : Game.State -> Float
averageResponseTimeInMillisecond state =
    0.1123


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
