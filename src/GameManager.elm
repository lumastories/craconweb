module GameManager
    exposing
        ( Game(..)
        , GameStatus(..)
        , InitConfig
        , init
        , updateTime
        , updateIndication
        , view
        )

import Html exposing (Html)
import Time exposing (Time)
import GenGame exposing (TrialResult(Complete, Continuing, ContinuingWithEvent))
import StopSignal
import GoNoGo
import DotProbe
import RespondSignal
import VisualSearch


type Game msg
    = StopSignal (GameData StopSignal.Settings StopSignal.Trial msg)
    | GoNoGo (GameData GoNoGo.Settings GoNoGo.Trial msg)
    | DotProbe (GameData DotProbe.Settings DotProbe.Trial msg)
    | RespondSignal (GameData (RespondSignal.Settings msg) RespondSignal.Trial msg)
    | VisualSearch (GameData VisualSearch.Settings VisualSearch.Trial msg)


type GameStatus msg
    = Running (Game msg)
    | Results (List (Maybe GenGame.Reason))


type State
    = Instructions Time
    | Trial
    | TrialRest Time
    | BlockRest (List (Maybe GenGame.Reason)) Time


type alias GameData settings trial msg =
    { remainingBlocks : List (List trial)
    , currTime : Time
    , blockResults : List (Maybe GenGame.Reason)
    , results : List (Maybe GenGame.Reason)
    , settings : settings
    , state : State
    , instructionsView : Html msg
    , trialRestView : Html msg
    , blockRestView : List (Maybe GenGame.Reason) -> Html msg
    , instructionsDuration : Time
    , trialRestDuration : Time
    , blockRestDuration : Time
    }


type alias InitConfig settings trial msg =
    { gameConstructor : GameData settings trial msg -> Game msg
    , blocks : List (List trial)
    , currTime : Time
    , settings : settings
    , instructionsView : Html msg
    , instructionsDuration : Time
    , trialRestView : Html msg
    , trialRestDuration : Time
    , blockRestView : List (Maybe GenGame.Reason) -> Html msg
    , blockRestDuration : Time
    }


init : InitConfig settings trial msg -> Game msg
init initConfig =
    initConfig.gameConstructor
        { remainingBlocks = initConfig.blocks
        , currTime = initConfig.currTime
        , blockResults = []
        , results = []
        , settings = initConfig.settings
        , state = Instructions initConfig.currTime
        , instructionsView = initConfig.instructionsView
        , trialRestView = initConfig.trialRestView
        , blockRestView = (initConfig.blockRestView)
        , instructionsDuration = initConfig.instructionsDuration
        , trialRestDuration = initConfig.trialRestDuration
        , blockRestDuration = initConfig.blockRestDuration
        }


updateTime : Time -> Game msg -> ( GameStatus msg, Cmd msg )
updateTime currTime game =
    case game of
        StopSignal data ->
            updateTimeHelper StopSignal StopSignal.updateTime currTime data

        GoNoGo data ->
            updateTimeHelper GoNoGo GoNoGo.updateTime currTime data

        DotProbe data ->
            updateTimeHelper DotProbe DotProbe.updateTime currTime data

        RespondSignal data ->
            updateTimeHelper RespondSignal RespondSignal.updateTime currTime data

        VisualSearch data ->
            updateTimeHelper VisualSearch VisualSearch.updateTime currTime data


updateTimeHelper :
    (GameData settings trial msg -> Game msg)
    -> (settings -> Time -> trial -> TrialResult trial msg)
    -> Time
    -> GameData settings trial msg
    -> ( GameStatus msg, Cmd msg )
updateTimeHelper gameConstructor updateF currTime data =
    updateHelper gameConstructor (updateF data.settings currTime) currTime data


updateHelper :
    (GameData settings trial msg -> Game msg)
    -> (trial -> TrialResult trial msg)
    -> Time
    -> GameData settings trial msg
    -> ( GameStatus msg, Cmd msg )
updateHelper gameConstructor updateF currTime data =
    case data.remainingBlocks of
        [] ->
            Results (List.reverse data.results) ! []

        [] :: blocks ->
            Running
                (gameConstructor
                    { data
                        | state = BlockRest (List.reverse data.blockResults) currTime
                        , blockResults = []
                        , results = data.blockResults ++ data.results
                    }
                )
                ! []

        (x :: xs) :: blocks ->
            case updateF x of
                Complete reason ->
                    Running
                        (gameConstructor
                            { data
                                | remainingBlocks = xs :: blocks
                                , results = reason :: data.results
                                , currTime = currTime
                                , state =
                                    -- TODO A bit hacky, need to figure out a better way, but it should be fine.
                                    if List.isEmpty xs then
                                        Trial
                                    else
                                        TrialRest currTime
                            }
                        )
                        ! []

                Continuing trial ->
                    Running
                        (gameConstructor
                            { data
                                | remainingBlocks = (trial :: xs) :: blocks
                                , currTime = currTime
                            }
                        )
                        ! []

                ContinuingWithEvent trial event ->
                    Running
                        (gameConstructor
                            { data
                                | remainingBlocks = (trial :: xs) :: blocks
                                , currTime = currTime
                            }
                        )
                        ! [ event ]


updateDirectionIndication : GenGame.Direction -> Game msg -> ( GameStatus msg, Cmd msg )
updateDirectionIndication indication game =
    case game of
        GoNoGo data ->
            updateIndicationHelper GoNoGo GoNoGo.updateIndication indication data

        DotProbe data ->
            updateIndicationHelper DotProbe DotProbe.updateIndication indication data

        _ ->
            Running game ! []


updateIndication : Game msg -> ( GameStatus msg, Cmd msg )
updateIndication game =
    case game of
        StopSignal data ->
            updateHelper
                StopSignal
                (StopSignal.updateIndication data.currTime)
                data.currTime
                data

        RespondSignal data ->
            updateHelper
                RespondSignal
                (RespondSignal.updateIndication data.currTime)
                data.currTime
                data

        _ ->
            Running game ! []


updateIntIndication : Int -> Game msg -> ( GameStatus msg, Cmd msg )
updateIntIndication indication game =
    case game of
        VisualSearch data ->
            updateIndicationHelper VisualSearch VisualSearch.updateIndication indication data

        _ ->
            Running game ! []


updateIndicationHelper :
    (GameData settings trial msg -> Game msg)
    -> (Time -> indication -> trial -> TrialResult trial msg)
    -> indication
    -> GameData settings trial msg
    -> ( GameStatus msg, Cmd msg )
updateIndicationHelper gameConstructor updateF indication data =
    updateHelper gameConstructor (updateF data.currTime indication) data.currTime data



-- TODO Needing this (Int -> msg) like this is smelly.


view : (Int -> msg) -> Game msg -> Html msg
view intMsg game =
    case game of
        StopSignal data ->
            viewHelper StopSignal.view data

        GoNoGo data ->
            viewHelper GoNoGo.view data

        DotProbe data ->
            viewHelper DotProbe.view data

        RespondSignal data ->
            viewHelper RespondSignal.view data

        VisualSearch data ->
            viewHelper (VisualSearch.view intMsg) data


viewHelper : (trial -> Html msg) -> GameData settings trial msg -> Html msg
viewHelper viewF data =
    case data.state of
        Instructions time ->
            data.instructionsView

        TrialRest time ->
            data.trialRestView

        BlockRest blockResults time ->
            data.blockRestView blockResults

        Trial ->
            case data.remainingBlocks of
                [] ->
                    Html.text ""

                [] :: _ ->
                    Html.text ""

                (trial :: _) :: _ ->
                    viewF trial
