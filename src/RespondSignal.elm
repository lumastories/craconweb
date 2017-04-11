module RespondSignal exposing (..)

import GenGame
    exposing
        ( Direction
        , TrialResult(Continuing, ContinuingWithEvent, Complete)
        , Reason(GoSuccess, NoGoSuccess, IndicationTimeout, WrongIndication, IndicatedOnNoGo)
        , checkTransition
        )
import Html exposing (Html, img, text)
import Html.Attributes exposing (class, src)
import Time exposing (Time)


type alias Trial =
    { position : Direction
    , imageUrl : String
    , kind : Kind
    , stage : Stage
    , lastTransition : Time
    , audioDelay : Maybe Time
    , postAudioDuration : Time
    , reason : Maybe Reason
    }


type Stage
    = PicturePreAudio
    | PicturePostAudio
    | Feedback
    | FixationCross


type Kind
    = Go
    | NoGo


type alias Settings msg =
    { fixationCross : Time
    , feedback : Time
    , audioEvent : Cmd msg
    }


isGo : Kind -> Bool
isGo kind =
    case kind of
        Go ->
            True

        NoGo ->
            False


updateTime : Settings msg -> Time -> Trial -> TrialResult Trial msg
updateTime settings currTime trial =
    let
        trans =
            checkTransition trial currTime trial.lastTransition
    in
        case trial.stage of
            PicturePreAudio ->
                case trial.audioDelay of
                    Nothing ->
                        Continuing { trial | stage = PicturePostAudio }

                    Just audioDelay ->
                        trans audioDelay
                            (ContinuingWithEvent { trial | stage = PicturePostAudio } settings.audioEvent)

            PicturePostAudio ->
                let
                    reason =
                        if isGo trial.kind then
                            IndicationTimeout
                        else
                            NoGoSuccess
                in
                    trans trial.postAudioDuration
                        (Continuing { trial | stage = Feedback, reason = Just reason })

            Feedback ->
                trans settings.feedback
                    (Continuing { trial | stage = FixationCross })

            FixationCross ->
                trans settings.fixationCross
                    (Complete trial.reason)


updateIndication : Time -> Trial -> TrialResult Trial msg
updateIndication currTime trial =
    if trial.reason == Nothing && trial.stage == PicturePostAudio then
        if isGo trial.kind then
            Continuing { trial | stage = FixationCross, reason = Just (GoSuccess currTime) }
        else
            Continuing { trial | stage = FixationCross, reason = Just (IndicatedOnNoGo currTime) }
    else
        Continuing trial


view : Trial -> Html msg
view trial =
    case trial.stage of
        PicturePreAudio ->
            pictureView trial.imageUrl

        PicturePostAudio ->
            pictureView trial.imageUrl

        Feedback ->
            text ""

        FixationCross ->
            img [ src "fixationCrossUrl" ] []


pictureView : String -> Html msg
pictureView url =
    img [ src url ] []
