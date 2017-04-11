module RespondSignal exposing (..)

import GenGame
    exposing
        ( Direction
        , TrialResult(Continuing, ContinuingWithEvent, Complete)
        , Reason(GoSuccess, NoGoSuccess, IndicationTimeout, WrongIndication, IndicatedOnNoGo)
        , checkTransition
        , updateReason
        )
import Html exposing (Html, img, text)
import Html.Attributes exposing (class, src)
import Time exposing (Time)


type alias Trial =
    { position : Direction
    , imageUrl : String
    , kind : Kind
    , stage : Stage
    , audioDelay : Maybe Time
    , postAudioDuration : Time
    , reason : Maybe Reason
    }


type Stage
    = NotStarted
    | PicturePreAudio Time
    | PicturePostAudio Time
    | Feedback Time


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
            checkTransition trial currTime
    in
        case trial.stage of
            NotStarted ->
                Continuing ({ trial | stage = PicturePreAudio currTime })

            PicturePreAudio timeSince ->
                case trial.audioDelay of
                    Nothing ->
                        Continuing { trial | stage = PicturePostAudio currTime }

                    Just audioDelay ->
                        trans
                            timeSince
                            audioDelay
                            (ContinuingWithEvent
                                { trial | stage = PicturePostAudio currTime }
                                settings.audioEvent
                            )

            PicturePostAudio timeSince ->
                let
                    reason =
                        if isGo trial.kind then
                            IndicationTimeout
                        else
                            NoGoSuccess
                in
                    trans
                        timeSince
                        trial.postAudioDuration
                        (Continuing { trial | stage = Feedback currTime, reason = Just reason })

            Feedback timeSince ->
                trans timeSince
                    settings.feedback
                    (Complete trial.reason)


updateIndication : Time -> Trial -> TrialResult Trial msg
updateIndication currTime trial =
    case trial.stage of
        PicturePostAudio _ ->
            if isGo trial.kind then
                Continuing { trial | reason = updateReason (GoSuccess currTime) trial.reason }
            else
                Continuing { trial | reason = updateReason (IndicatedOnNoGo currTime) trial.reason }

        _ ->
            Continuing trial


view : Trial -> Html msg
view trial =
    case trial.stage of
        NotStarted ->
            pictureView trial.imageUrl

        PicturePreAudio _ ->
            pictureView trial.imageUrl

        PicturePostAudio _ ->
            pictureView trial.imageUrl

        Feedback _ ->
            text ""


pictureView : String -> Html msg
pictureView url =
    img [ src url ] []
