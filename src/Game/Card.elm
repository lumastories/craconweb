module Game.Card
    exposing
        ( Card
        , Continuation(Continue, Complete)
        , andThen
        , card
        , complete
        , layout
        , step
        )


type Card a layout input msg
    = Card
        { logic : input -> ( Continuation a layout input msg, Cmd msg )
        , layout : Maybe layout
        }


type Continuation a layout input msg
    = Continue (Card a layout input msg)
    | Complete a


andThen : input -> (a -> Card b layout input msg) -> Card a layout input msg -> Card b layout input msg
andThen initialize f (Card card) =
    let
        newLogic input =
            case card.logic input of
                ( Complete a, cmd1 ) ->
                    let
                        ( continuation, cmd2 ) =
                            step initialize (f a)
                    in
                        ( continuation, Cmd.batch [ cmd1, cmd2 ] )

                ( Continue newCard, cmd ) ->
                    ( Continue (andThen initialize f newCard), cmd )
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
