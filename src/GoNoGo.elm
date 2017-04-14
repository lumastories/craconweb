module GoNoGo exposing (..)

import GenGame
    exposing
        ( Direction(Left, Right)
        , TrialResult(Continuing, Complete)
        , Reason(GoSuccess, NoGoSuccess, IndicationTimeout, WrongIndication, IndicatedOnNoGo)
        , TrialFuns
        , checkTransition
        , updateReason
        , take
        , redCross
        , bounded
        )
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (class, src)
import List.Extra
import Random exposing (Generator)
import Random.Extra
import Random.List
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
    { blockCount : Int
    , responseCount : Int
    , nonResponseCount : Int
    , fillerCount : Int
    , pictureDuration : Time
    , durationIncrement : Time
    , minDuration : Time
    , maxDuration : Time
    , redCross : Time
    }


init :
    Settings
    -> List String
    -> List String
    -> List String
    -> Generator (List (List Trial))
init settings responseUrls nonResponseUrls fillerUrls =
    Random.Extra.andThen3
        (\sGo sNoGo ( sGoFill, sNoGoFill ) ->
            let
                go =
                    sGo
                        |> List.take settings.responseCount
                        |> List.map (initTrial Go)

                noGo =
                    sNoGo
                        |> List.take settings.nonResponseCount
                        |> List.map (initTrial NoGo)

                goFill =
                    sGoFill
                        |> List.take ((settings.fillerCount + 1) // 2)
                        |> List.map (initTrial Go)

                noGoFill =
                    sNoGoFill
                        |> List.take (settings.fillerCount // 2)
                        |> List.map (initTrial NoGo)
            in
                List.concat [ go, noGo, goFill, noGoFill ]
                    |> List.repeat 2
                    |> List.concat
                    |> directionalize
                    |> Random.andThen Random.List.shuffle
                    |> Random.map (List.Extra.greedyGroupsOf settings.blockCount)
        )
        (Random.List.shuffle responseUrls)
        (Random.List.shuffle nonResponseUrls)
        (Random.List.shuffle fillerUrls |> Random.map halve)


halve : List a -> ( List a, List a )
halve xs =
    let
        len =
            (List.length xs + 1) // 2
    in
        ( List.take len xs, List.drop len xs )


directionalize : List (Direction -> a) -> Generator (List a)
directionalize xs =
    xs
        |> List.map
            (\x ->
                Random.Extra.choice Left Right
                    |> Random.map ((<|) x)
            )
        |> Random.Extra.combine


initTrial : Kind -> String -> Direction -> Trial
initTrial kind imageUrl direction =
    { position = direction
    , imageUrl = imageUrl
    , kind = kind
    , stage = NotStarted
    , reason = Nothing
    }


trialFuns : TrialFuns Settings Trial msg
trialFuns =
    { getTrialImages = always []
    , updateTime = updateTime
    , updateIndication = GenGame.defaultUpdateIndication
    , updateDirectionIndication = updateIndication
    , updateIntIndication = GenGame.defaultUpdateWithIndication
    , view = view
    }


isGo : Kind -> Bool
isGo kind =
    case kind of
        Go ->
            True

        NoGo ->
            False


updateTime : Settings -> Time -> Trial -> ( TrialResult Trial msg, Settings )
updateTime settings currTime trial =
    let
        trans =
            checkTransition trial currTime
    in
        case trial.stage of
            NotStarted ->
                ( Continuing { trial | stage = Picture currTime }, settings )

            Picture timeSince ->
                if isGo trial.kind then
                    ( trans timeSince
                        settings.pictureDuration
                        (Continuing
                            { trial
                                | stage = RedCross currTime
                                , reason =
                                    updateReason IndicationTimeout trial.reason
                            }
                        )
                    , settings
                    )
                else
                    ( trans timeSince
                        settings.pictureDuration
                        (Complete (updateReason NoGoSuccess trial.reason))
                    , { settings
                        | pictureDuration =
                            bounded
                                settings.minDuration
                                settings.maxDuration
                                (settings.pictureDuration + settings.durationIncrement)
                      }
                    )

            RedCross timeSince ->
                ( trans timeSince
                    settings.redCross
                    (Complete trial.reason)
                , settings
                )


updateIndication : Settings -> Time -> Direction -> Trial -> ( TrialResult Trial msg, Settings )
updateIndication settings currTime direction trial =
    ( updateIndicationHelper currTime direction trial, settings )


updateIndicationHelper : Time -> Direction -> Trial -> TrialResult Trial msg
updateIndicationHelper currTime direction trial =
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
                Continuing
                    { trial
                        | stage = RedCross currTime
                        , reason = updateReason (IndicatedOnNoGo currTime) trial.reason
                    }

        _ ->
            Continuing trial


view : Trial -> Html msg
view trial =
    border trial.kind [ content trial.stage trial.imageUrl trial.position ]


content : Stage -> String -> Direction -> Html msg
content stage url position =
    case stage of
        NotStarted ->
            pictureView url position

        Picture _ ->
            pictureView url position

        RedCross _ ->
            div [ class "container has-text-centered" ]
                [ redCross ]


pictureView : String -> Direction -> Html msg
pictureView url position =
    case position of
        Left ->
            img [ class "squeezed", src url ] []

        Right ->
            img [ class "is-pulled-right squeezed", src url ] []


border : Kind -> List (Html msg) -> Html msg
border kind =
    if isGo kind then
        div [ class "solidBorder sized" ]
    else
        div [ class "dashedBorder sized" ]
