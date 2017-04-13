module GameManager
    exposing
        ( Game(..)
        , GameStatus(..)
        , InitConfig
        , init
        , updateTime
        , updateIndication
        , updateIntIndication
        , updateDirectionIndication
        , view
        )

import Html exposing (Html)
import Random exposing (Generator)
import Random.Extra
import Time exposing (Time)


-- games

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


type alias GameData settings trial msg =
    { remainingBlocks : List (Blocks trial)
    , currTime : Time
    , prevTime : Time
    , startTime : Time
    , blockResults : List (Maybe GenGame.Reason)
    , results : List (Maybe GenGame.Reason)
    , settings : settings
    , instructionsView : Html msg
    , trialRestView : Html msg
    , blockRestView : List (Maybe GenGame.Reason) -> Html msg
    , reportView : List (Maybe GenGame.Reason) -> Html msg
    }


type alias InitConfig settings trial msg =
    { gameConstructor : GameData settings trial msg -> Game msg
    , blocks : List (List trial)
    , currTime : Time
    , settings : settings
    , instructionsView : Html msg
    , trialRestView : Html msg
    , trialRestDuration : Time
    , trialRestJitter : Time
    , blockRestView : List (Maybe GenGame.Reason) -> Html msg
    , blockRestDuration : Time
    , reportView : List (Maybe GenGame.Reason) -> Html msg
    }


type Trials trial
    = TrialRest Time
    | TrialActive trial


type Blocks trial
    = BlockRest Time
    | BlockActive (List (Trials trial))
    | Instructions
    | Report


init : InitConfig settings trial msg -> Generator (Game msg)
init initConfig =
    padBlocks
        initConfig.blockRestDuration
        initConfig.trialRestDuration
        initConfig.trialRestJitter
        initConfig.blocks
        |> Random.map
            (\blocks ->
                initConfig.gameConstructor
                    { remainingBlocks = blocks
                    , currTime = initConfig.currTime
                    , prevTime = initConfig.currTime
                    , startTime = initConfig.currTime
                    , blockResults = []
                    , results = []
                    , settings = initConfig.settings
                    , instructionsView = initConfig.instructionsView
                    , trialRestView = initConfig.trialRestView
                    , blockRestView = (initConfig.blockRestView)
                    , reportView = initConfig.reportView
                    }
            )


padBlocks : Time -> Time -> Time -> List (List trial) -> Generator (List (Blocks trial))
padBlocks blockRest trialRest trialJitter blocks =
    blocks
        |> List.map
            (\block ->
                block
                    |> List.map (Random.Extra.constant << TrialActive)
                    |> List.intersperse
                        (Random.map TrialRest
                            (Random.float trialRest (trialRest + trialJitter))
                        )
                    |> Random.Extra.combine
                    |> Random.map BlockActive
            )
        |> Random.Extra.combine
        |> Random.map
            (\blocks ->
                blocks
                    |> (\l -> l ++ [ Report ])
                    |> List.intersperse (BlockRest blockRest)
                    |> (::) Instructions
            )


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
    case dismissInfo game of
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

        newGame ->
            Running newGame ! []


dismissInfo : Game msg -> Game msg
dismissInfo game =
    let
        proceed gameConstructor data =
            case data.remainingBlocks of
                Instructions :: blocks ->
                    gameConstructor { data | remainingBlocks = blocks }

                Report :: blocks ->
                    gameConstructor { data | remainingBlocks = blocks }

                _ ->
                    game
    in
        case game of
            StopSignal data ->
                proceed StopSignal data

            GoNoGo data ->
                proceed GoNoGo data

            DotProbe data ->
                proceed DotProbe data

            RespondSignal data ->
                proceed RespondSignal data

            VisualSearch data ->
                proceed VisualSearch data


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


updateHelper :
    (GameData settings trial msg -> Game msg)
    -> (trial -> TrialResult trial msg)
    -> Time
    -> GameData settings trial msg
    -> ( GameStatus msg, Cmd msg )
updateHelper gameConstructor updateF currTime data =
    let
        reRun =
            updateHelper gameConstructor updateF currTime

        noOp =
            Running (gameConstructor { data | currTime = currTime }) ! []

        durationSwitch duration remaining =
            if currTime - data.prevTime >= duration then
                reRun { data | remainingBlocks = remaining, prevTime = currTime }
            else
                noOp
    in
        case data.remainingBlocks of
            [] ->
                Results (List.reverse data.results) ! []

            (BlockRest duration) :: blocks ->
                durationSwitch duration blocks

            Instructions :: blocks ->
                noOp

            Report :: blocks ->
                noOp

            (BlockActive block) :: blocks ->
                case block of
                    [] ->
                        reRun { data | remainingBlocks = blocks }

                    (TrialRest duration) :: trials ->
                        durationSwitch duration (BlockActive trials :: blocks)

                    (TrialActive trial) :: trials ->
                        case updateF trial of
                            Complete reason ->
                                Running
                                    (gameConstructor
                                        { data
                                            | remainingBlocks = BlockActive trials :: blocks
                                            , results = reason :: data.results
                                            , currTime = currTime
                                        }
                                    )
                                    ! []

                            Continuing trial ->
                                Running
                                    (gameConstructor
                                        { data
                                            | remainingBlocks =
                                                BlockActive (TrialActive trial :: trials) :: blocks
                                            , currTime = currTime
                                        }
                                    )
                                    ! []

                            ContinuingWithEvent trial event ->
                                Running
                                    (gameConstructor
                                        { data
                                            | remainingBlocks =
                                                BlockActive (TrialActive trial :: trials) :: blocks
                                            , currTime = currTime
                                        }
                                    )
                                    ! [ event ]



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
    case data.remainingBlocks of
        [] ->
            Html.text ""

        (BlockRest duration) :: blocks ->
            data.blockRestView data.blockResults

        Instructions :: blocks ->
            data.instructionsView

        Report :: blocks ->
            data.reportView data.results

        (BlockActive block) :: blocks ->
            case block of
                [] ->
                    Html.text ""

                (TrialRest duration) :: trials ->
                    data.trialRestView

                (TrialActive trial) :: trials ->
                    viewF trial
