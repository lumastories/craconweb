module GameManager
    exposing
        ( GameData
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
import GenGame exposing (TrialFuns)
import Random exposing (Generator)
import Random.Extra
import Time exposing (Time)


-- games

import GenGame exposing (TrialResult(Complete, Continuing, ContinuingWithEvent))


type GameStatus settings trial msg
    = Running (GameData settings trial msg)
    | Results (List (Maybe GenGame.Reason))


type alias GameData settings trial msg =
    { remainingBlocks : List (Blocks trial)
    , currTime : Time
    , prevTime : Time
    , startTime : Time
    , maxDuration : Time
    , blockResults : List (Maybe GenGame.Reason)
    , results : List (Maybe GenGame.Reason)
    , settings : settings
    , instructionsView : Html msg
    , trialRestView : Html msg
    , blockRestView : List (Maybe GenGame.Reason) -> Html msg
    , reportView : List (Maybe GenGame.Reason) -> Html msg
    , trialFuns : TrialFuns settings trial msg
    }


type alias InitConfig settings trial msg =
    { blocks : List (List trial)
    , currTime : Time
    , maxDuration : Time
    , settings : settings
    , instructionsView : Html msg
    , trialRestView : Html msg
    , trialRestDuration : Time
    , trialRestJitter : Time
    , blockRestView : List (Maybe GenGame.Reason) -> Html msg
    , blockRestDuration : Time
    , reportView : List (Maybe GenGame.Reason) -> Html msg
    , trialFuns : TrialFuns settings trial msg
    }


type Trials trial
    = TrialRest Time
    | TrialActive trial


type Blocks trial
    = BlockRest Time
    | BlockActive (List (Trials trial))
    | Instructions
    | Report


init : InitConfig settings trial msg -> Generator (GameData settings trial msg)
init initConfig =
    padBlocks
        initConfig.blockRestDuration
        initConfig.trialRestDuration
        initConfig.trialRestJitter
        initConfig.blocks
        |> Random.map
            (\blocks ->
                { remainingBlocks = blocks
                , currTime = initConfig.currTime
                , prevTime = initConfig.currTime
                , maxDuration = initConfig.maxDuration
                , startTime = initConfig.currTime
                , blockResults = []
                , results = []
                , settings = initConfig.settings
                , instructionsView = initConfig.instructionsView
                , trialRestView = initConfig.trialRestView
                , blockRestView = (initConfig.blockRestView)
                , reportView = initConfig.reportView
                , trialFuns = initConfig.trialFuns
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


updateTime : Time -> GameData settings trial msg -> ( GameStatus settings trial msg, Cmd msg )
updateTime currTime data =
    updateTimeHelper data.trialFuns.updateTime currTime data


updateTimeHelper :
    (settings -> Time -> trial -> ( TrialResult trial msg, settings ))
    -> Time
    -> GameData settings trial msg
    -> ( GameStatus settings trial msg, Cmd msg )
updateTimeHelper updateF currTime data =
    updateHelper (updateF data.settings currTime) currTime data


updateDirectionIndication :
    GenGame.Direction
    -> GameData settings trial msg
    -> ( GameStatus settings trial msg, Cmd msg )
updateDirectionIndication indication data =
    updateIndicationHelper data.trialFuns.updateDirectionIndication indication data


updateIndication : GameData settings trial msg -> ( GameStatus settings trial msg, Cmd msg )
updateIndication data =
    updateHelper
        (data.trialFuns.updateIndication data.settings data.currTime)
        data.currTime
        (dismissInfo data)


dismissInfo : GameData settings trial msg -> GameData settings trial msg
dismissInfo data =
    case data.remainingBlocks of
        Instructions :: blocks ->
            { data | remainingBlocks = blocks, startTime = data.currTime }

        Report :: blocks ->
            { data | remainingBlocks = blocks }

        _ ->
            data


updateIntIndication : Int -> GameData settings trial msg -> ( GameStatus settings trial msg, Cmd msg )
updateIntIndication indication data =
    updateIndicationHelper data.trialFuns.updateIntIndication indication data


updateIndicationHelper :
    (settings -> Time -> indication -> trial -> ( TrialResult trial msg, settings ))
    -> indication
    -> GameData settings trial msg
    -> ( GameStatus settings trial msg, Cmd msg )
updateIndicationHelper updateF indication data =
    updateHelper (updateF data.settings data.currTime indication) data.currTime data


updateHelper :
    (trial -> ( TrialResult trial msg, settings ))
    -> Time
    -> GameData settings trial msg
    -> ( GameStatus settings trial msg, Cmd msg )
updateHelper updateF currTime data =
    let
        reRunF =
            updateHelper updateF currTime

        reRun newData =
            if currTime - newData.startTime >= newData.maxDuration then
                reRunF { newData | remainingBlocks = [ Report ] }
            else
                reRunF newData

        noOp =
            Running { data | currTime = currTime } ! []

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

            (BlockActive []) :: blocks ->
                reRun { data | remainingBlocks = blocks }

            (BlockActive ((TrialRest duration) :: trials)) :: blocks ->
                durationSwitch duration (BlockActive trials :: blocks)

            (BlockActive ((TrialActive trial) :: trials)) :: blocks ->
                case updateF trial of
                    ( Complete reason, settings ) ->
                        reRun
                            { data
                                | remainingBlocks = BlockActive trials :: blocks
                                , results = reason :: data.results
                                , currTime = currTime
                                , prevTime = currTime
                                , settings = settings
                            }

                    ( Continuing trial, settings ) ->
                        Running
                            { data
                                | remainingBlocks =
                                    BlockActive (TrialActive trial :: trials) :: blocks
                                , currTime = currTime
                                , settings = settings
                            }
                            ! []

                    ( ContinuingWithEvent trial event, settings ) ->
                        Running
                            { data
                                | remainingBlocks =
                                    BlockActive (TrialActive trial :: trials) :: blocks
                                , currTime = currTime
                                , settings = settings
                            }
                            ! [ event ]


view : GameData settings trial msg -> Html msg
view data =
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
                    data.trialFuns.view trial
