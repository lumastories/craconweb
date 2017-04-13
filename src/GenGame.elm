module GenGame
    exposing
        ( Direction(..)
        , TrialResult(..)
        , Reason(..)
        , TrialFuns
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
        )

import Html exposing (Html, div, text)
import Random exposing (Generator)
import Task exposing (Task)
import Time exposing (Time)


type Reason
    = GoSuccess Time
    | NoGoSuccess
    | IndicationTimeout
    | WrongIndication Time
    | IndicatedOnNoGo Time


type Direction
    = Left
    | Right


type TrialResult trial msg
    = Complete (Maybe Reason)
    | Continuing trial
    | ContinuingWithEvent trial (Cmd msg)


type alias TrialFuns settings trial msg =
    { getTrialImages : trial -> List String
    , updateTime : settings -> Time -> trial -> ( TrialResult trial msg, settings )
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
    text "X"


blackDot : Html msg
blackDot =
    text "•"


fixationCross : Html msg
fixationCross =
    text "+"


defaultUpdateIndication : settings -> time -> trial -> ( TrialResult trial msg, settings )
defaultUpdateIndication settings _ trial =
    ( Continuing trial, settings )


defaultUpdateWithIndication : settings -> time -> indication -> trial -> ( TrialResult trial msg, settings )
defaultUpdateWithIndication settings _ _ trial =
    ( Continuing trial, settings )


bounded : comparable -> comparable -> comparable -> comparable
bounded low high x =
    x
        |> min low
        |> max high
