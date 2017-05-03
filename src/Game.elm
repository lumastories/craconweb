module Game exposing (..)

import Game.Card as Card exposing (Continuation(Complete, Continue))
import Random exposing (Generator)
import Random.Extra
import Time exposing (Time)


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
    | LeftRight BorderType Image Image
    | SelectGrid BorderType Int Int (List Image)
    | RedCross BorderType


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
    = BeginSession Int Time
    | EndSession Time
    | BeginTrial Time
    | EndTrial Time
    | BeginDisplay (Maybe Layout) Time
    | PlaySound Time
    | AcceptIndication Bool Time
    | AcceptDirection Direction Direction Time
    | AcceptSelection Int Int Time
    | Timeout Bool Time


type alias State =
    { sessionStart : Maybe Time
    , trialStart : Time
    , currTime : Time
    , log : List LogEntry
    , trialResult : Maybe Bool
    }


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
    Card.andThen isTimeout Initialize


andThen : (State -> Game msg) -> Game msg -> Game msg
andThen =
    Card.andThen (always False) Initialize


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
    { state | trialStart = state.currTime, trialResult = Nothing }


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
        ( Indication, Nothing ) ->
            ( True
            , { state
                | log = AcceptIndication desired state.currTime :: state.log
                , trialResult = Just desired
              }
            )

        _ ->
            ( True, state )


timeout : Time -> Logic
timeout expiration state _ =
    ( state.trialStart + expiration > state.currTime, state )


resultTimeout : Bool -> Time -> Logic
resultTimeout desired expiration state input =
    case ( state.trialResult, timeout expiration state input ) of
        ( Nothing, ( False, newState ) ) ->
            ( False
            , { state
                | log = Timeout desired state.currTime :: newState.log
                , trialResult = Just desired
              }
            )

        ( Just _, ( False, newState ) ) ->
            ( False, newState )

        ( _, ( True, newState ) ) ->
            ( True, newState )


showRedCross : Logic
showRedCross state input =
    case Debug.log "showRedCross: state.trialResult" (state.trialResult) of
        Nothing ->
            ( False, state )

        Just True ->
            ( False, state )

        Just False ->
            ( True, state )


updateCurrTime : Logic
updateCurrTime state input =
    case input of
        Tick time ->
            ( True, { state | currTime = time } )

        _ ->
            ( True, state )


emptyState : Time -> State
emptyState time =
    { sessionStart = Nothing
    , trialStart = time
    , currTime = time
    , log = []
    , trialResult = Nothing
    }
