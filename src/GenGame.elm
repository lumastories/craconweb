module GenGame
    exposing
        ( Direction(..)
        , TrialResult(..)
        , Reason(..)
        , TrialFuns
        , AggregatedReason
        , checkTransition
        , updateReason
        , take
        , generatorToTask
        , redCross
        , blackDot
        , fixationCross
        , defaultUpdateIndication
        , defaultUpdateWithIndication
        , bounded
        , wrapper
        , aggregateReasons
        )

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Random exposing (Generator)
import Task exposing (Task)
import Time exposing (Time)
import Maybe.Extra


type Reason
    = GoSuccess Time
    | NoGoSuccess
    | IndicationTimeout
    | WrongIndication Time
    | IndicatedOnNoGo Time
    | DirectionSuccess Direction (List String) Time
    | SelectionSuccess Int (List String) Time


type Direction
    = Left
    | Right


type TrialResult trial msg
    = Complete (Maybe Reason)
    | Continuing trial
    | ContinuingWithEvent trial (Cmd msg)


type alias TrialFuns settings trial msg =
    { updateTime : settings -> Time -> trial -> ( TrialResult trial msg, settings )
    , updateIndication :
        settings
        -> Time
        -> trial
        -> ( TrialResult trial msg, settings )
    , updateDirectionIndication :
        settings
        -> Time
        -> Direction
        -> trial
        -> ( TrialResult trial msg, settings )
    , updateIntIndication :
        settings
        -> Time
        -> Int
        -> trial
        -> ( TrialResult trial msg, settings )
    , view : trial -> Html msg
    }


checkTransition :
    trial
    -> Time
    -> Time
    -> Time
    -> TrialResult trial msg
    -> TrialResult trial msg
checkTransition trial currTime lastTransition duration expired =
    if currTime - lastTransition >= duration then
        expired
    else
        Continuing trial


updateReason : a -> Maybe a -> Maybe a
updateReason new old =
    case old of
        Nothing ->
            Just new

        Just _ ->
            old


take : String -> Int -> List a -> Result String (List a)
take message n xs =
    let
        ys =
            List.take n xs
    in
        if List.length ys == n then
            Ok ys
        else
            Err message


generatorToTask : Generator a -> Task x a
generatorToTask generator =
    Time.now
        |> Task.map (round >> Random.initialSeed >> Random.step generator >> Tuple.first)


redCross : Html msg
redCross =
    div [ class "redCross" ] [ text "X" ]


blackDot : Html msg
blackDot =
    text "•"


fixationCross : Html msg
fixationCross =
    text "+"


wrapper : List (Html msg) -> Html msg
wrapper kids =
    div [ class "gameWrapper" ] kids


defaultUpdateIndication : settings -> time -> trial -> ( TrialResult trial msg, settings )
defaultUpdateIndication settings _ trial =
    ( Continuing trial, settings )


defaultUpdateWithIndication : settings -> time -> indication -> trial -> ( TrialResult trial msg, settings )
defaultUpdateWithIndication settings _ _ trial =
    ( Continuing trial, settings )


bounded : comparable -> comparable -> comparable -> comparable
bounded low high x =
    x
        |> min high
        |> max low


type alias AggregatedReason =
    { averageResponseTimeResult : Result String Float
    , percentCorrect : Float
    }


type alias CurrentTally =
    { totalResponseTime : Float
    , responseTimeCount : Int
    , totalCorrect : Int
    , totalTrials : Int
    }


aggregateReasons : List (Maybe Reason) -> AggregatedReason
aggregateReasons reasons =
    reasons
        |> List.foldl aggregator
            { totalResponseTime = 0
            , responseTimeCount = 0
            , totalCorrect = 0
            , totalTrials = 0
            }
        |> aggregateTally


aggregateTally : CurrentTally -> AggregatedReason
aggregateTally tally =
    { averageResponseTimeResult = Ok <| (tally.totalResponseTime / toFloat tally.responseTimeCount) / 1000.0
    , percentCorrect = toFloat tally.totalCorrect / toFloat tally.totalTrials * 100
    }


aggregator : Maybe Reason -> CurrentTally -> CurrentTally
aggregator reason currentTally =
    let
        responseTime =
            getResponseTime reason

        hasResponseTime =
            Maybe.Extra.isJust responseTime
    in
        { currentTally
            | totalResponseTime =
                currentTally.totalResponseTime + Maybe.withDefault 0 responseTime
            , responseTimeCount =
                if hasResponseTime then
                    currentTally.responseTimeCount + 1
                else
                    currentTally.responseTimeCount
            , totalCorrect =
                if isCorrect reason then
                    currentTally.totalCorrect + 1
                else
                    currentTally.totalCorrect
            , totalTrials = currentTally.totalTrials + 1
        }


getResponseTime : Maybe Reason -> Maybe Time
getResponseTime reason =
    case reason of
        Nothing ->
            Nothing

        Just (GoSuccess time) ->
            Just time

        Just NoGoSuccess ->
            Nothing

        Just IndicationTimeout ->
            Nothing

        Just (WrongIndication time) ->
            Just time

        Just (IndicatedOnNoGo time) ->
            Just time

        Just (DirectionSuccess _ _ time) ->
            Just time

        Just (SelectionSuccess _ _ time) ->
            Just time


isCorrect : Maybe Reason -> Bool
isCorrect reason =
    case reason of
        Nothing ->
            False

        Just (GoSuccess _) ->
            True

        Just NoGoSuccess ->
            True

        Just IndicationTimeout ->
            False

        Just (WrongIndication time) ->
            False

        Just (IndicatedOnNoGo time) ->
            True

        Just (DirectionSuccess _ _ time) ->
            True

        Just (SelectionSuccess _ _ time) ->
            True
