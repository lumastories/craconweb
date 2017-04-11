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
    { responseCount : Int
    , nonResponseCount : Int
    , blockCount : Int
    , blockSize : Int
    , pictureNoBorder : Time
    , pictureBorder : Time
    , redCross : Time
    }


init : Settings -> List String -> List String -> Result String (Generator (List (List Trial)))
init settings responseUrls nonResponseUrls =
    Result.map2
        (\resp nResp ->
            let
                gs =
                    List.map (initTrial Go) responseUrls

                ngs =
                    List.map (initTrial NoGo) nonResponseUrls
            in
                List.map
                    (\_ -> mkBlock settings.blockSize gs ngs)
                    (List.range 1 settings.blockCount)
                    |> Random.Extra.combine
        )
        (take "Not enough response pictures to begin." settings.responseCount responseUrls)
        (take "Not enough non-response pictures to begin." settings.nonResponseCount nonResponseUrls)


mkBlock : Int -> List Trial -> List Trial -> Generator (List Trial)
mkBlock size gos nogos =
    Random.Extra.andThen2
        (\gs ngs ->
            Random.List.shuffle
                (List.take (size // 2) ngs
                    ++ List.take (size - (size // 2)) gs
                )
        )
        (Random.List.shuffle gos)
        (Random.List.shuffle nogos)


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
