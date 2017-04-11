module GenGame
    exposing
        ( Direction(Left, Right)
        , TrialResult(Complete, Continuing, ContinuingWithEvent)
        , checkTransition
        , updateReason
        )

import Time exposing (Time)


type Direction
    = Left
    | Right


type TrialResult reason trial msg
    = Complete (Maybe reason)
    | Continuing trial
    | ContinuingWithEvent trial (Cmd msg)


checkTransition :
    trial
    -> Time
    -> Time
    -> Time
    -> TrialResult reason trial msg
    -> TrialResult reason trial msg
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
