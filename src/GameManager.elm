module GameManager exposing (..)

import Time exposing (Time)
import GenGame exposing (TrialResult(Complete, Continuing, ContinuingWithEvent))
import StopSignal
import GoNoGo
import DotProbe
import RespondSignal
import VisualSearch


type Game msg
    = StopSignal (GameData StopSignal.Settings StopSignal.Trial)
    | GoNoGo (GameData GoNoGo.Settings GoNoGo.Trial)
    | DotProbe (GameData DotProbe.Settings DotProbe.Trial)
    | RespondSignal (GameData (RespondSignal.Settings msg) RespondSignal.Trial)
    | VisualSearch (GameData VisualSearch.Settings VisualSearch.Trial)


type GameStatus msg
    = Running (Game msg)
    | Results (List (Maybe Reason))


type Reason
    = StopSignalReason StopSignal.Reason
    | GoNoGoReason GoNoGo.Reason
    | DotProbeReason DotProbe.Reason
    | RespondSignalReason RespondSignal.Reason
    | VisualSearchReason VisualSearch.Reason


type alias GameData settings trial =
    { remainingTrials : List trial
    , currTime : Time
    , results : List (Maybe Reason)
    , settings : settings
    }


updateTime : Time -> Game msg -> ( GameStatus msg, Cmd msg )
updateTime currTime game =
    case game of
        StopSignal data ->
            updateTimeHelper StopSignal StopSignalReason StopSignal.updateTime currTime data

        GoNoGo data ->
            updateTimeHelper GoNoGo GoNoGoReason GoNoGo.updateTime currTime data

        DotProbe data ->
            updateTimeHelper DotProbe DotProbeReason DotProbe.updateTime currTime data

        RespondSignal data ->
            updateTimeHelper RespondSignal RespondSignalReason RespondSignal.updateTime currTime data

        VisualSearch data ->
            updateTimeHelper VisualSearch VisualSearchReason VisualSearch.updateTime currTime data


updateTimeHelper :
    (GameData settings trial -> Game msg)
    -> (reason -> Reason)
    -> (settings -> Time -> trial -> TrialResult reason trial msg)
    -> Time
    -> GameData settings trial
    -> ( GameStatus msg, Cmd msg )
updateTimeHelper gameConstructor reasonConstructor updateF currTime data =
    updateHelper gameConstructor reasonConstructor (updateF data.settings currTime) currTime data


updateHelper :
    (GameData settings trial -> Game msg)
    -> (reason -> Reason)
    -> (trial -> TrialResult reason trial msg)
    -> Time
    -> GameData settings trial
    -> ( GameStatus msg, Cmd msg )
updateHelper gameConstructor reasonConstructor updateF currTime data =
    case data.remainingTrials of
        [] ->
            Results (List.reverse data.results) ! []

        x :: xs ->
            case updateF x of
                Complete reason ->
                    Running
                        (gameConstructor
                            { data
                                | remainingTrials = xs
                                , results = (Maybe.map reasonConstructor reason) :: data.results
                                , currTime = currTime
                            }
                        )
                        ! []

                Continuing trial ->
                    Running (gameConstructor { data | remainingTrials = trial :: xs, currTime = currTime })
                        ! []

                ContinuingWithEvent trial event ->
                    Running (gameConstructor { data | remainingTrials = trial :: xs, currTime = currTime })
                        ! [ event ]


updateDirectionIndication : GenGame.Direction -> Game msg -> ( GameStatus msg, Cmd msg )
updateDirectionIndication indication game =
    case game of
        StopSignal data ->
            updateIndicationHelper StopSignal StopSignalReason StopSignal.updateIndication indication data

        GoNoGo data ->
            updateIndicationHelper GoNoGo GoNoGoReason GoNoGo.updateIndication indication data

        DotProbe data ->
            updateIndicationHelper DotProbe DotProbeReason DotProbe.updateIndication indication data

        _ ->
            Running game ! []


updateIndication : Game msg -> ( GameStatus msg, Cmd msg )
updateIndication game =
    case game of
        RespondSignal data ->
            updateHelper RespondSignal RespondSignalReason (RespondSignal.updateIndication data.currTime) data.currTime data

        _ ->
            Running game ! []


updateIntIndication : Int -> Game msg -> ( GameStatus msg, Cmd msg )
updateIntIndication indication game =
    case game of
        VisualSearch data ->
            updateIndicationHelper VisualSearch VisualSearchReason VisualSearch.updateIndication indication data

        _ ->
            Running game ! []


updateIndicationHelper :
    (GameData settings trial -> Game msg)
    -> (reason -> Reason)
    -> (Time -> indication -> trial -> TrialResult reason trial msg)
    -> indication
    -> GameData settings trial
    -> ( GameStatus msg, Cmd msg )
updateIndicationHelper gameConstructor reasonConstructor updateF indication data =
    updateHelper gameConstructor reasonConstructor (updateF data.currTime indication) data.currTime data
