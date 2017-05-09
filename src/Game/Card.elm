module Game.Card
    exposing
        ( Card
        , Continuation(Continue, Complete)
        , andThen
        , card
        , complete
        , layout
        , step
        , unwrap
        )


type Card a layout input msg
    = Card
        { logic : input -> ( Continuation a layout input msg, Cmd msg )
        , layout : Maybe layout
        }


type Continuation a layout input msg
    = Continue a (Card a layout input msg)
    | Complete a


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
    in
        Card { card | logic = newLogic }


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

        Complete a ->
            a
