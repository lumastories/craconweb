module DotProbe exposing (..)

import GenGame
    exposing
        ( Direction
        , TrialResult(Continuing, Complete)
        , Reason(GoSuccess, WrongIndication)
        , checkTransition
        )
import Html exposing (Html, text)
import Time exposing (Time)


type alias Trial =
    { probePosition : Direction
    , leftImageUrl : String
    , rightImageUrl : String
    , stage : Stage
    , reason : Maybe Reason
    }


type Stage
    = NotStarted
    | FixationCross Time
    | Pictures Time
    | Probe Time


type alias Settings =
    { pictures : Time
    , fixationCross : Time
    , probe : Time
    }


updateTime : Settings -> Time -> Trial -> TrialResult Trial msg
updateTime settings currTime trial =
    let
        trans =
            checkTransition trial currTime
    in
        case trial.stage of
            NotStarted ->
                Continuing { trial | stage = FixationCross currTime }

            FixationCross timeSince ->
                trans settings.fixationCross timeSince (Continuing { trial | stage = Pictures currTime })

            Pictures timeSince ->
                trans settings.pictures timeSince (Continuing { trial | stage = Probe currTime })

            Probe timeSince ->
                trans settings.probe timeSince (Complete trial.reason)


updateIndication : Time -> Direction -> Trial -> TrialResult Trial msg
updateIndication currTime direction trial =
    case trial.stage of
        Probe _ ->
            if trial.probePosition == direction then
                Complete (Just (GoSuccess currTime))
            else
                Complete (Just (WrongIndication currTime))

        _ ->
            Continuing trial


view : Trial -> Html msg
view trial =
    case trial.stage of
        NotStarted ->
            text ""

        FixationCross _ ->
            text ""

        Pictures _ ->
            text ""

        Probe _ ->
            text ""
