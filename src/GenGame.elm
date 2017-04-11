module GenGame
    exposing
        ( Direction(..)
        , TrialResult(..)
        , Reason(..)
        , checkTransition
        , updateReason
        , take
        , generatorToTask
        )

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


checkTransition :
    trial
    -> Time
    -> Time
    -> Time
    -> TrialResult trial msg
    -> TrialResult trial msg
checkTransition trial currTime lastTransition duration expired =
    let
        lastT =
            if lastTransition < 1 then
                currTime
            else
                lastTransition
    in
        if currTime - lastT >= duration then
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
