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
    , lastTransition : Time
    }


type Stage
    = Pictures
    | Probe
    | FixationCross


type alias Settings =
    { pictures : Time
    , fixationCross : Time
    }


updateTime : Settings -> Time -> Trial -> TrialResult Trial msg
updateTime settings currTime trial =
    let
        trans =
            checkTransition trial currTime trial.lastTransition
    in
        case trial.stage of
            FixationCross ->
                trans settings.fixationCross (Continuing { trial | stage = Pictures })

            Pictures ->
                trans settings.pictures (Continuing { trial | stage = Probe })

            Probe ->
                Continuing trial


updateIndication : Time -> Direction -> Trial -> TrialResult Trial msg
updateIndication currTime direction trial =
    if trial.stage == Probe then
        if trial.probePosition == direction then
            Complete (Just (GoSuccess currTime))
        else
            Complete (Just (WrongIndication currTime))
    else
        Continuing trial


view : Trial -> Html msg
view trial =
    case trial.stage of
        FixationCross ->
            text ""

        Pictures ->
            text ""

        Probe ->
            text ""
