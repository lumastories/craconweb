module GoNoGo exposing (..)

import GenGame
    exposing
        ( Direction
        , TrialResult(Continuing, Complete)
        , Reason(GoSuccess, NoGoSuccess, IndicationTimeout, WrongIndication, IndicatedOnNoGo)
        , checkTransition
        , updateReason
        )
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (class, src)
import Time exposing (Time)


type alias Trial =
    { position : Direction
    , imageUrl : String
    , kind : Kind
    , stage : Stage
    , lastTransition : Time
    , reason : Maybe Reason
    }


type Stage
    = Picture
    | RedCross
    | Pause


type Kind
    = Go
    | NoGo


type alias Settings =
    { picture : Time
    , redCross : Time
    , pause : Time
    }


isGo : Kind -> Bool
isGo kind =
    case kind of
        Go ->
            True

        NoGo ->
            False


updateTime : Settings -> Time -> Trial -> TrialResult Trial msg
updateTime settings currTime trial =
    let
        trans =
            checkTransition trial currTime trial.lastTransition
    in
        case trial.stage of
            Picture ->
                let
                    ( stage, reason ) =
                        if isGo trial.kind then
                            ( RedCross, IndicationTimeout )
                        else
                            ( Pause, NoGoSuccess )
                in
                    trans settings.picture
                        (Continuing { trial | stage = stage, reason = updateReason reason trial.reason })

            RedCross ->
                trans settings.redCross
                    (Continuing { trial | stage = Pause, lastTransition = currTime })

            Pause ->
                trans settings.pause
                    (Complete trial.reason)


updateIndication : Time -> Direction -> Trial -> TrialResult Trial msg
updateIndication currTime direction trial =
    if trial.stage == Picture then
        if isGo trial.kind then
            if trial.position == direction then
                Continuing
                    { trial
                        | stage = Pause
                        , reason = updateReason (GoSuccess currTime) trial.reason
                    }
            else
                Continuing
                    { trial
                        | stage = RedCross
                        , reason = updateReason (WrongIndication currTime) trial.reason
                    }
        else
            Continuing { trial | reason = updateReason (IndicatedOnNoGo currTime) trial.reason }
    else
        Continuing trial


view : Trial -> Html msg
view trial =
    border trial.kind [ content trial.stage trial.imageUrl ]


content : Stage -> String -> Html msg
content stage url =
    case stage of
        Picture ->
            img [ src url ] []

        RedCross ->
            img [ src "redCrossUrl" ] []

        Pause ->
            text ""


border : Kind -> List (Html msg) -> Html msg
border kind =
    if isGo kind then
        div [ class "solidBorder" ]
    else
        div [ class "dashedBorder" ]
