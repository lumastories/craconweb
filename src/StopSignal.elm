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
    , lastTransition : Time
    , reason : Maybe Reason
    }


type Stage
    = PictureNoBorder
    | PictureBorder
    | RedCross


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
    , stage = PictureNoBorder
    , lastTransition = 0
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
            checkTransition trial currTime trial.lastTransition
    in
        case trial.stage of
            PictureNoBorder ->
                trans settings.pictureNoBorder
                    (Continuing { trial | stage = PictureBorder })

            PictureBorder ->
                if isGo trial.kind then
                    trans settings.pictureBorder
                        (Continuing
                            { trial
                                | stage = RedCross
                                , reason = updateReason IndicationTimeout trial.reason
                            }
                        )
                else
                    Complete (Just NoGoSuccess)

            RedCross ->
                trans settings.redCross
                    (Complete trial.reason)


updateIndication : Time -> Trial -> TrialResult Trial msg
updateIndication currTime trial =
    if trial.stage == PictureBorder then
        if isGo trial.kind then
            Continuing { trial | reason = updateReason (GoSuccess currTime) trial.reason }
        else
            Continuing { trial | reason = updateReason (IndicatedOnNoGo currTime) trial.reason }
    else
        Continuing trial


view : Trial -> Html msg
view trial =
    case trial.stage of
        PictureNoBorder ->
            img [ src trial.imageUrl ] []

        PictureBorder ->
            border trial.kind [ img [ src trial.imageUrl ] [] ]

        RedCross ->
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
