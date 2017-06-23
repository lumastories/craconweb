module Game.Card
    exposing
        ( Card
        , Continuation(..)
        , andThen
        , andThenBreak
        , card
        , complete
        , layout
        , step
        , unwrap
        )

import Time exposing (Time)


type Card state layout input msg
    = Card
        { logic : input -> ( Continuation state layout input msg, Cmd msg )
        , layout : Maybe layout
        }


type Continuation state layout input msg
    = Continue state (Card state layout input msg)
    | Break state (Card state layout input msg)
    | Complete state


andThen : (state -> Bool) -> (state -> state) -> input -> (state -> Card state layout input msg) -> Card state layout input msg -> Card state layout input msg
andThen isTimeout resetSegmentStart initialize f (Card card) =
    let
        newLogic input =
            case card.logic input of
                ( Complete state, cmd1 ) ->
                    if isTimeout state then
                        ( Complete state, cmd1 )
                    else
                        let
                            ( continuation, cmd2 ) =
                                step initialize (f (resetSegmentStart state))
                        in
                            ( continuation, Cmd.batch [ cmd1, cmd2 ] )

                ( Continue state newCard, cmd ) ->
                    ( Continue
                        state
                        (andThen isTimeout resetSegmentStart initialize f newCard)
                    , cmd
                    )

                ( Break state newCard, cmd ) ->
                    ( Continue
                        state
                        (andThen isTimeout resetSegmentStart initialize f newCard)
                    , cmd
                    )
    in
        Card { card | logic = newLogic }


andThenBreak :
    { breakCard : state -> Card state layout input msg
    , breakDuration : Time
    , shouldBreak : state -> Bool
    , isFinish : state -> Bool
    , isRest : Card state layout input msg -> Bool
    , resetSegmentStart : state -> state
    , resetBlockStart : Time -> state -> state
    , initialize : input
    }
    -> (state -> Card state layout input msg)
    -> Card state layout input msg
    -> Card state layout input msg
andThenBreak ({ breakCard, isRest, breakDuration, shouldBreak, isFinish, resetSegmentStart, resetBlockStart, initialize } as args) f (Card card) =
    let
        newLogic input =
            case card.logic input of
                ( Complete state, cmd1 ) ->
                    let
                        updatedState =
                            resetSegmentStart state
                    in
                        case ( shouldBreak state, isFinish state ) of
                            ( True, False ) ->
                                ( updatedState
                                    |> resetBlockStart breakDuration
                                    |> breakCard
                                    |> Break updatedState
                                , cmd1
                                )

                            ( True, True ) ->
                                ( Complete state, cmd1 )

                            ( False, _ ) ->
                                let
                                    ( continuation, cmd2 ) =
                                        step initialize (f updatedState)
                                in
                                    ( continuation, Cmd.batch [ cmd1, cmd2 ] )

                ( Break state newCard, cmd1 ) ->
                    continuingFromBreak args cmd1 newCard f state

                ( Continue state newCard, cmd ) ->
                    ( Continue
                        state
                        (andThenBreak args f newCard)
                    , cmd
                    )
    in
        Card { card | logic = newLogic }


continuingFromBreak :
    { breakCard : state -> Card state layout input msg
    , breakDuration : Time
    , initialize : input
    , isFinish : state -> Bool
    , isRest : Card state layout input msg -> Bool
    , resetBlockStart : Time -> state -> state
    , resetSegmentStart : state -> state
    , shouldBreak : state -> Bool
    }
    -> Cmd msg
    -> Card state layout input msg
    -> (state -> Card state layout input msg)
    -> state
    -> ( Continuation state layout input msg, Cmd msg )
continuingFromBreak args cmd newCard f state =
    let
        ( continuation, cmd2 ) =
            step args.initialize (f (args.resetSegmentStart state))

        contCard =
            continuationCard continuation
    in
        case ( contCard, Maybe.map args.isRest contCard ) of
            ( Just card, Just True ) ->
                let
                    ( nextContinuation, cmd3 ) =
                        step args.initialize (f (args.resetSegmentStart (unwrapContinuation continuation)))
                in
                    ( nextContinuation
                        |> continuationCard
                        |> Maybe.map (\nextCard -> Continue state (andThenBreak args f nextCard))
                        |> Maybe.withDefault (Complete state)
                    , Cmd.batch [ cmd, cmd2, cmd3 ]
                    )

            _ ->
                ( Continue
                    state
                    (andThenBreak args f newCard)
                , cmd
                )


card : Maybe layout -> (input -> ( Continuation a layout input msg, Cmd msg )) -> Card a layout input msg
card layout logic =
    Card { layout = layout, logic = logic }


complete : a -> Card a layout input msg
complete x =
    Card { logic = always ( Complete x, Cmd.none ), layout = Nothing }


layout : Card a layout input msg -> Maybe layout
layout (Card card) =
    card.layout


step : input -> Card a layout input msg -> ( Continuation a layout input msg, Cmd msg )
step input (Card card) =
    card.logic input


unwrap : input -> Card a layout input msg -> a
unwrap initialize card =
    case card of
        Card { logic } ->
            let
                ( continuation, _ ) =
                    logic initialize
            in
                unwrapContinuation continuation


unwrapContinuation : Continuation a layout input msg -> a
unwrapContinuation continuation =
    case continuation of
        Continue a _ ->
            a

        Break a _ ->
            a

        Complete a ->
            a


continuationCard : Continuation a layout input msg -> Maybe (Card a layout input msg)
continuationCard continuation =
    case continuation of
        Continue _ layout ->
            Just layout

        Break _ layout ->
            Just layout

        Complete _ ->
            Nothing
