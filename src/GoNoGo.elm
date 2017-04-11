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
    , reason : Maybe Reason
    }


type Stage
    = NotStarted
    | Picture Time
    | RedCross Time


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
            checkTransition trial currTime
    in
        case trial.stage of
            NotStarted ->
                Continuing { trial | stage = Picture currTime }

            Picture timeSince ->
                if isGo trial.kind then
                    trans timeSince
                        settings.picture
                        (Continuing
                            { trial
                                | stage = RedCross currTime
                                , reason =
                                    updateReason IndicationTimeout trial.reason
                            }
                        )
                else
                    Complete (updateReason IndicationTimeout trial.reason)

            RedCross timeSince ->
                trans timeSince
                    settings.redCross
                    (Complete trial.reason)


updateIndication : Time -> Direction -> Trial -> TrialResult Trial msg
updateIndication currTime direction trial =
    case trial.stage of
        Picture timeSince ->
            if isGo trial.kind then
                if trial.position == direction then
                    Complete (updateReason (GoSuccess currTime) trial.reason)
                else
                    Continuing
                        { trial
                            | stage = RedCross currTime
                            , reason = updateReason (WrongIndication currTime) trial.reason
                        }
            else
                Continuing { trial | reason = updateReason (IndicatedOnNoGo currTime) trial.reason }

        _ ->
            Continuing trial



-- TODO doesn't handle left/right


view : Trial -> Html msg
view trial =
    border trial.kind [ content trial.stage trial.imageUrl ]


content : Stage -> String -> Html msg
content stage url =
    case stage of
        NotStarted ->
            pictureView url

        Picture _ ->
            pictureView url

        RedCross _ ->
            img [ src "redCrossUrl" ] []


pictureView : String -> Html msg
pictureView url =
    img [ src url ] []


border : Kind -> List (Html msg) -> Html msg
border kind =
    if isGo kind then
        div [ class "solidBorder" ]
    else
        div [ class "dashedBorder" ]
