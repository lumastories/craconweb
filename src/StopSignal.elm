module StopSignal exposing (..)

import GenGame
    exposing
        ( TrialResult(Continuing, Complete)
        , Reason(GoSuccess, NoGoSuccess, IndicationTimeout, IndicatedOnNoGo)
        , checkTransition
        , updateReason
        , take
        )
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (class, src)
import List.Extra
import Random exposing (Generator)
import Random.Extra
import Random.List
import Time exposing (Time)


type alias Trial =
    { imageUrl : String
    , kind : Kind
    , stage : Stage
    , reason : Maybe Reason
    }


type Stage
    = NotStarted
    | PictureNoBorder Time
    | PictureBorder Time
    | RedCross Time


type Kind
    = Go
    | NoGo


type alias Settings =
    { blockResponseCount : Int
    , blockNonResponseCount : Int
    , pictureNoBorder : Time
    , pictureBorder : Time
    , redCross : Time
    }


init : Settings -> List String -> List String -> Generator (List (List Trial))
init settings responseUrls nonResponseUrls =
    Random.map2
        (\sGo sNoGo ->
            let
                go =
                    List.Extra.groupsOf settings.blockResponseCount sGo

                noGo =
                    List.Extra.groupsOf settings.blockNonResponseCount sNoGo
            in
                List.Extra.zip go noGo
                    |> List.map (\( a, b ) -> Random.List.shuffle (a ++ b))
                    |> Random.Extra.combine
        )
        (Random.List.shuffle (List.map (initTrial Go) responseUrls))
        (Random.List.shuffle (List.map (initTrial NoGo) nonResponseUrls))
        |> Random.andThen identity


initTrial : Kind -> String -> Trial
initTrial kind imageUrl =
    { imageUrl = imageUrl
    , kind = kind
    , stage = NotStarted
    , reason = Nothing
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
                Continuing { trial | stage = PictureNoBorder currTime }

            PictureNoBorder timeSince ->
                trans timeSince
                    settings.pictureNoBorder
                    (Continuing { trial | stage = PictureBorder currTime })

            PictureBorder timeSince ->
                if isGo trial.kind then
                    trans timeSince
                        settings.pictureBorder
                        (Continuing
                            { trial
                                | stage = RedCross currTime
                                , reason = updateReason IndicationTimeout trial.reason
                            }
                        )
                else
                    Complete (Just NoGoSuccess)

            RedCross timeSince ->
                trans timeSince
                    settings.redCross
                    (Complete trial.reason)


updateIndication : Time -> Trial -> TrialResult Trial msg
updateIndication currTime trial =
    case trial.stage of
        PictureBorder timeSince ->
            if isGo trial.kind then
                Continuing
                    { trial
                        | reason = updateReason (GoSuccess (currTime - timeSince)) trial.reason
                    }
            else
                Continuing
                    { trial
                        | reason = updateReason (IndicatedOnNoGo (currTime - timeSince)) trial.reason
                    }

        _ ->
            Continuing trial


view : Trial -> Html msg
view trial =
    case trial.stage of
        NotStarted ->
            img [ src trial.imageUrl ] []

        PictureNoBorder _ ->
            img [ src trial.imageUrl ] []

        PictureBorder _ ->
            border trial.kind [ img [ src trial.imageUrl ] [] ]

        RedCross _ ->
            img [ src "redCrossUrl" ] []


border : Kind -> List (Html msg) -> Html msg
border kind =
    if isGo kind then
        div [ class "solidBorder" ]
    else
        div [ class "dashedBorder" ]


instructions : Html msg
instructions =
    text "StopSignal instructions missing."


blockRestView : List (Maybe Reason) -> Html msg
blockRestView reasons =
    text "Calculate block score and display."
