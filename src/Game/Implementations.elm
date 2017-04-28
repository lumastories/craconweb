module Game.Implementations exposing (..)

import Game.Card exposing (complete)
import Game
    exposing
        ( Game
        , Image
        , Layout(..)
        , Border(..)
        , LogEntry(..)
        , State
        , andThen
        , emptyState
        , game
        , log
        , addRests
        , info
        , onIndication
        , timeout
        , resultTimeout
        )
import Random exposing (Generator)
import Random.List
import Time exposing (Time)


stopSignalInit : Time -> Time -> String -> List Image -> List Image -> Int -> Time -> Game msg
stopSignalInit borderDelay totalDuration infoString responseImages nonResponseImages seedInt currTime =
    let
        gos =
            List.map (\img -> stopSignalTrial borderDelay totalDuration img True) responseImages

        noGos =
            List.map (\img -> stopSignalTrial borderDelay totalDuration img False) nonResponseImages

        trials =
            gos ++ noGos
    in
        Random.List.shuffle trials
            |> Random.andThen (addRests Nothing 500 0)
            |> Random.map
                (\trials ->
                    (info infoString :: log (BeginSession seedInt) :: trials)
                        |> List.foldl Game.andThen (Game.Card.complete (emptyState currTime))
                )
            |> (\generator -> Random.step generator (Random.initialSeed seedInt))
            |> Tuple.first


stopSignalTrial : Time -> Time -> Image -> Bool -> State -> Game msg
stopSignalTrial borderDelay totalDuration image goTrial state =
    let
        border =
            if goTrial then
                Blue
            else
                Grey

        borderless =
            Just (Single None image)

        bordered =
            Just (Single border image)
    in
        complete { state | trialResult = Nothing, trialStart = state.currTime }
            |> andThen (log (BeginDisplay borderless))
            |> andThen (game [ onIndication False, timeout borderDelay ] borderless)
            |> andThen (log (BeginDisplay bordered))
            |> andThen
                (game
                    [ onIndication goTrial
                    , resultTimeout (not goTrial) totalDuration
                    ]
                    bordered
                )
            |> andThen (game [ timeout totalDuration ] bordered)
