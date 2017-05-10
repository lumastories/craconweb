module Game exposing (..)

import Game.Card as Card exposing (Continuation(Complete, Continue))
import Random exposing (Generator)
import Random.Extra
import Time exposing (Time)


type GameState msg
    = NotPlaying
    | Playing (Game msg)
    | Finished State


type alias Game msg =
    Card.Card State Layout Input msg


type alias Continuation msg =
    Card.Continuation State Layout Input msg


type Input
    = Initialize
    | Tick Time
    | Indication
    | Select Int
    | Direction Direction


type Direction
    = Left
    | Right


type Layout
    = Info BorderType String
    | Single BorderType Image
    | LeftOrRight BorderType Direction Image
    | LeftRight BorderType Image Image
    | SelectGrid BorderType { columns : Int, images : List Image, goIndex : Int }
    | RedCross BorderType
    | Fixation BorderType
    | Probe BorderType Direction


type BorderType
    = None
    | Grey
    | Blue
    | Black
    | Dashed


type alias Image =
    { id : String
    , url : String
    }


type LogEntry
    = BeginSession { seed : Int } Time
    | EndSession Time
    | BeginTrial Time
    | EndTrial Time
    | BeginDisplay (Maybe Layout) Time
    | BeginInput Time
    | AcceptIndication { desired : Bool } Time
    | AcceptDirection { desired : Direction, actual : Direction } Time
    | AcceptSelection { desired : Int, actual : Int } Time
    | Timeout { desired : Bool } Time


type alias State =
    { sessionStart : Maybe Time
    , trialStart : Time
    , segmentStart : Time
    , currTime : Time
    , log : List LogEntry
    , trialResult : Result
    , currentSeed : Random.Seed
    }


type Result
    = NoResult
    | BoolResult Bool
    | SelectResult { result : Bool, answer : Maybe Int }


type alias Logic =
    State -> Input -> ( Bool, State )


unwrap : Game msg -> State
unwrap =
    Card.unwrap Initialize


segment : List Logic -> Maybe Layout -> State -> Game msg
segment logics layout state =
    let
        combined =
            oneOf (updateCurrTime :: logics) state
    in
        Card.card
            layout
            (\input ->
                case combined input of
                    ( True, newState ) ->
                        ( Continue newState (segment logics layout newState), Cmd.none )

                    ( False, newState ) ->
                        ( Complete newState, Cmd.none )
            )


andThenCheckTimeout : (State -> Bool) -> (State -> Game msg) -> Game msg -> Game msg
andThenCheckTimeout isTimeout =
    Card.andThen isTimeout resetSegmentStart Initialize


andThen : (State -> Game msg) -> Game msg -> Game msg
andThen =
    Card.andThen (always False) resetSegmentStart Initialize


resetSegmentStart : State -> State
resetSegmentStart state =
    { state | segmentStart = state.currTime }


oneOf : List Logic -> Logic
oneOf logics state input =
    List.foldl
        (\logic ( continue, newState ) ->
            if continue then
                logic newState input
            else
                ( continue, newState )
        )
        ( True, state )
        logics


log : (Time -> LogEntry) -> State -> Game msg
log logEntry state =
    Card.card
        Nothing
        (\_ ->
            ( Complete { state | log = logEntry state.currTime :: state.log }
            , Cmd.none
            )
        )


rest : Maybe Layout -> Time -> State -> Game msg
rest layout expiration state =
    log (BeginDisplay layout) (startTrial state)
        |> andThen (segment [ timeout expiration ] layout)


addRests : Maybe Layout -> Time -> Time -> List (State -> Game msg) -> Generator (List (State -> Game msg))
addRests layout min jitter trials =
    trials
        |> List.map Random.Extra.constant
        |> List.intersperse (Random.float min (min + jitter) |> Random.map (rest layout))
        |> Random.Extra.combine


startSession : State -> Game msg
startSession state =
    Card.complete { state | sessionStart = Just state.currTime }


startTrial : State -> State
startTrial state =
    { state | trialStart = state.currTime, trialResult = NoResult }


info : String -> State -> Game msg
info infoString state =
    segment [ advanceOnIndication ] (Just (Info None infoString)) state


advanceOnIndication : Logic
advanceOnIndication state input =
    case input of
        Indication ->
            ( False, state )

        _ ->
            ( True, state )


onIndication : Bool -> Logic
onIndication desired state input =
    case ( input, state.trialResult ) of
        ( Indication, NoResult ) ->
            ( True
            , { state
                | log = AcceptIndication { desired = desired } state.currTime :: state.log
                , trialResult = BoolResult desired
              }
            )

        _ ->
            ( True, state )


onSelect : Int -> Logic
onSelect desiredIndex state input =
    case ( input, state.trialResult ) of
        ( Select actualIndex, NoResult ) ->
            ( False
            , { state
                | log = AcceptSelection { desired = desiredIndex, actual = actualIndex } state.currTime :: state.log
                , trialResult = SelectResult { result = desiredIndex == actualIndex, answer = Just actualIndex }
              }
            )

        _ ->
            ( True, state )


onDirection : Bool -> Direction -> Logic
onDirection desired desiredDirection state input =
    case ( input, state.trialResult ) of
        ( Direction actualDirection, NoResult ) ->
            ( False
            , { state
                | log = AcceptDirection { desired = desiredDirection, actual = actualDirection } state.currTime :: state.log
                , trialResult = BoolResult (desired && desiredDirection == actualDirection)
              }
            )

        _ ->
            ( True, state )


timeout : Time -> Logic
timeout expiration state _ =
    ( state.trialStart + expiration > state.currTime, state )


timeoutFromSegmentStart : Time -> Logic
timeoutFromSegmentStart expiration state _ =
    ( state.segmentStart + expiration > state.currTime, state )


selectTimeout : Time -> Logic
selectTimeout expiration state input =
    case ( state.trialResult, timeout expiration state input ) of
        ( NoResult, ( False, newState ) ) ->
            ( False
            , { state
                | log = Timeout { desired = False } state.currTime :: newState.log
                , trialResult = SelectResult { result = False, answer = Nothing }
              }
            )

        ( BoolResult _, ( False, newState ) ) ->
            ( False, newState )

        ( SelectResult _, ( False, newState ) ) ->
            ( False, newState )

        ( _, ( True, newState ) ) ->
            ( True, newState )


resultTimeout : Bool -> Time -> Logic
resultTimeout desired expiration state input =
    case ( state.trialResult, timeout expiration state input ) of
        ( NoResult, ( False, newState ) ) ->
            ( False
            , { state
                | log = Timeout { desired = desired } state.currTime :: newState.log
                , trialResult = BoolResult desired
              }
            )

        ( BoolResult _, ( False, newState ) ) ->
            ( False, newState )

        ( SelectResult _, ( False, newState ) ) ->
            ( False, newState )

        ( _, ( True, newState ) ) ->
            ( True, newState )


trialFailed : Logic
trialFailed state input =
    case state.trialResult of
        NoResult ->
            ( False, state )

        BoolResult True ->
            ( False, state )

        BoolResult False ->
            ( True, state )

        SelectResult { result } ->
            ( not result, state )


showZoom : Logic
showZoom state input =
    case state.trialResult of
        NoResult ->
            ( False, state )

        BoolResult True ->
            ( False, state )

        BoolResult False ->
            ( False, state )

        SelectResult _ ->
            ( True, state )


updateCurrTime : Logic
updateCurrTime state input =
    case input of
        Tick time ->
            ( True, { state | currTime = time } )

        _ ->
            ( True, state )


emptyState : Int -> Time -> State
emptyState initialSeed time =
    { sessionStart = Nothing
    , trialStart = time
    , segmentStart = time
    , currTime = time
    , log = []
    , trialResult = NoResult
    , currentSeed = Random.initialSeed initialSeed
    }


isPlaying : GameState msg -> Bool
isPlaying gameState =
    case gameState of
        Playing _ ->
            True

        NotPlaying ->
            False

        Finished _ ->
            False


leftOrRight : Generator Direction
leftOrRight =
    Random.bool
        |> Random.map
            (\bool ->
                if bool then
                    Left
                else
                    Right
            )
