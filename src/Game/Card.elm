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


andThen : (a -> Bool) -> input -> (a -> Card a layout input msg) -> Card a layout input msg -> Card a layout input msg
andThen isTimeout initialize f (Card card) =
    let
        newLogic input =
            case card.logic input of
                ( Complete a, cmd1 ) ->
                    if isTimeout a then
                        ( Complete a, cmd1 )
                    else
                        let
                            ( continuation, cmd2 ) =
                                step initialize (f a)
                        in
                            ( continuation, Cmd.batch [ cmd1, cmd2 ] )

                ( Continue a newCard, cmd ) ->
                    ( Continue a (andThen isTimeout initialize f newCard), cmd )
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
