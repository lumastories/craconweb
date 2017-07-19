module Game exposing (..)

import Game.Card as Card exposing (Continuation(Complete, Continue))
import Random exposing (Generator)
import Random.Extra
import RemoteData
import Time exposing (Time)


type GameState msg
    = NotPlaying
    | Loading (Game msg) (RemoteData.WebData Session)
    | Playing { game : Game msg, session : Session }
    | Saving State Session (RemoteData.WebData ( Session, List Cycle ))
    | Saved State { session : Session, cycles : List Cycle }


type alias Session =
    { id : String
    , userId : String
    , gameId : String
    , seed : Int
    , start : Time
    , end : Maybe Time
    , jitter : Bool
    }


type alias Cycle =
    { id : Maybe String
    , sessionId : String
    , sort : Int
    , fixation : Maybe Time
    , selection : Maybe Time
    , pictures : Maybe Time
    , redcross : Maybe Time
    , probe : Maybe Time
    , border : Maybe Time
    , timeout : Maybe Time
    , rest : Maybe Time
    , width : Maybe Int
    , height : Maybe Int
    , blue : Bool
    , gray : Bool
    , dash : Bool
    , targetIndex : Int
    , selectedIndex : Int
    , startIndex : Int
    , images : List String
    }


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


flipDirection : Direction -> Direction
flipDirection direction =
    case direction of
        Left ->
            Right

        Right ->
            Left


directionToIndex : Direction -> Int
directionToIndex direction =
    case direction of
        Left ->
            0

        Right ->
            1


type Layout
    = Info BorderType String
    | Single BorderType Image
    | LeftOrRight BorderType Direction Image
    | LeftRight BorderType Direction Image Image
    | SelectGrid BorderType { columns : Int, images : List Image, goIndex : Int }
    | RedCross BorderType
    | Fixation BorderType
    | Probe BorderType Direction
    | Interval
    | Rest


type BorderType
    = None
    | Gray
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
    , blockStart : Maybe Time
    , trialStart : Time
    , segmentStart : Time
    , currTime : Time
    , log : List LogEntry
    , trialResult : Result
    , currentSeed : Random.Seed
    , blockCounter : Int
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


andThenRest : { restDuration : Time, shouldRest : State -> Bool, isFinish : State -> Bool } -> (State -> Game msg) -> Game msg -> Game msg
andThenRest { restDuration, shouldRest, isFinish } =
    Card.andThenRest
        { restCard = rest restDuration
        , restDuration = restDuration
        , shouldRest = shouldRest
        , isFinish = isFinish
        , isInterval = isInterval
        , resetSegmentStart = resetSegmentStart
        , resetBlockStart = resetBlockStart
        , initialize = Initialize
        }


isInterval : Game msg -> Bool
isInterval game =
    case Card.layout game of
        Just Interval ->
            True

        _ ->
            False


andThen : (State -> Game msg) -> Game msg -> Game msg
andThen =
    Card.andThen (always False) resetSegmentStart Initialize


resetSegmentStart : State -> State
resetSegmentStart state =
    { state | segmentStart = state.currTime }


resetBlockStart : Time -> State -> State
resetBlockStart restDuration state =
    { state
        | blockStart = Just (state.currTime + restDuration)
        , blockCounter = state.blockCounter + 1
    }


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
log =
    logWithCondition (always True)


logWithCondition : (State -> Bool) -> (Time -> LogEntry) -> State -> Game msg
logWithCondition enabled logEntry state =
    Card.card
        Nothing
        (\_ ->
            ( Complete
                (if enabled state then
                    { state | log = logEntry state.currTime :: state.log }
                 else
                    state
                )
            , Cmd.none
            )
        )


rest : Time -> State -> Game msg
rest duration state =
    log (BeginDisplay (Just Rest)) (startTrial state)
        |> andThen (segment [ timeoutFromSegmentStart duration ] (Just Rest))


interval : Time -> State -> Game msg
interval expiration state =
    log (BeginDisplay (Just Interval)) (startTrial state)
        |> andThen (segment [ timeout expiration ] (Just Interval))


addIntervals : Maybe Layout -> Time -> Time -> List (State -> Game msg) -> Generator (List (State -> Game msg))
addIntervals layout min jitter trials =
    trials
        |> List.map Random.Extra.constant
        |> List.intersperse (Random.float min (min + jitter) |> Random.map interval)
        |> Random.Extra.combine


startSession : State -> Game msg
startSession state =
    Card.complete
        { state
            | sessionStart = Just state.currTime
            , blockStart = Just state.currTime
        }


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


isFailed : State -> Bool
isFailed state =
    case state.trialResult of
        NoResult ->
            False

        BoolResult True ->
            False

        BoolResult False ->
            True

        SelectResult { result } ->
            not result


trialFailed : Logic
trialFailed state input =
    state
        |> isFailed
        |> flip (,) state


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
    , blockStart = Nothing
    , trialStart = time
    , segmentStart = time
    , currTime = time
    , log = []
    , trialResult = NoResult
    , currentSeed = Random.initialSeed initialSeed
    , blockCounter = 0
    }


isPlaying : GameState msg -> Bool
isPlaying gameState =
    case gameState of
        Playing _ ->
            True

        Loading _ _ ->
            False

        NotPlaying ->
            False

        Saving _ _ _ ->
            False

        Saved _ _ ->
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
