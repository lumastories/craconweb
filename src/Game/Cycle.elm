module Game.Cycle exposing (generate)

import Game
import Time exposing (Time)


generate : String -> List Game.LogEntry -> List Game.Cycle
generate sessionId logEntries =
    logEntries
        |> List.foldr (fillCycles sessionId) []


fillCycles : String -> Game.LogEntry -> List Game.Cycle -> List Game.Cycle
fillCycles sessionId logEntry cycles =
    case logEntry of
        Game.BeginSession { seed } time ->
            cycles

        Game.EndSession time ->
            cycles

        Game.BeginTrial time ->
            beginCycle { sessionId = sessionId, time = time, sort = List.length cycles } cycles

        Game.EndTrial time ->
            endCycle time cycles

        Game.BeginDisplay maybeLayout time ->
            beginDisplay { sessionId = sessionId, maybeLayout = maybeLayout, time = time } cycles

        Game.BeginInput time ->
            cycles

        Game.AcceptIndication { desired } time ->
            acceptIndication { desired = desired, time = time } cycles

        Game.AcceptDirection { desired, actual } time ->
            acceptDirection { desired = desired, actual = actual, time = time } cycles

        Game.AcceptSelection { desired, actual } time ->
            acceptSelection { desired = desired, actual = actual, time = time } cycles

        Game.Timeout { desired } time ->
            timeout time cycles


beginCycle : { sessionId : String, time : Time, sort : Int } -> List Game.Cycle -> List Game.Cycle
beginCycle { sessionId, time, sort } cycles =
    { id = Nothing
    , sessionId = sessionId
    , sort = sort
    , fixation = Nothing
    , selection = Nothing
    , pictures = Nothing
    , redcross = Nothing
    , probe = Nothing
    , border = Nothing
    , timeout = Nothing
    , rest = Nothing
    , width = Nothing
    , height = Nothing
    , blue = False
    , grey = False
    , dash = False
    , probeIndex = Nothing
    , targetIndex = 0
    , selectedIndex = 0
    , startIndex = 0
    , images = []
    }
        :: cycles


endCycle : Time -> List Game.Cycle -> List Game.Cycle
endCycle time cycles =
    case cycles of
        [] ->
            cycles

        cycle :: tail ->
            { cycle | rest = Just time } :: tail


beginDisplay : { sessionId : String, time : Time, maybeLayout : Maybe Game.Layout } -> List Game.Cycle -> List Game.Cycle
beginDisplay { sessionId, time, maybeLayout } cycles =
    case ( maybeLayout, cycles ) of
        ( _, [] ) ->
            cycles

        ( Nothing, _ ) ->
            cycles

        ( Just (Game.Info _ _), _ ) ->
            cycles

        ( Just (Game.Single borderType image), cycle :: tail ) ->
            ({ cycle
                | pictures = Just time
                , images = [ image.id ]
                , startIndex = 0
                , targetIndex = 0
                , selectedIndex = 0
             }
                :: tail
            )
                |> beginBorder { borderType = borderType, time = time }

        ( Just (Game.LeftOrRight borderType direction image), cycle :: tail ) ->
            let
                targetIndex =
                    Game.directionToIndex direction
            in
                { cycle
                    | pictures = Just time
                    , images = [ image.id ]
                    , startIndex = targetIndex
                    , targetIndex = targetIndex
                    , selectedIndex = 0
                }
                    :: tail
                    |> beginBorder { borderType = borderType, time = time }

        ( Just (Game.LeftRight borderType direction leftImage rightImage), cycle :: tail ) ->
            { cycle
                | pictures = Just time
                , images = [ leftImage.id, rightImage.id ]
                , startIndex = 0
                , targetIndex = Game.directionToIndex direction
            }
                :: tail
                |> beginBorder { borderType = borderType, time = time }

        ( Just (Game.SelectGrid borderType { columns, images, goIndex }), cycle :: tail ) ->
            { cycle
                | pictures = Just time
                , images = List.map .id images
                , startIndex = 0
                , targetIndex = goIndex
                , selectedIndex = 0
            }
                :: tail
                |> beginBorder { borderType = borderType, time = time }

        ( Just (Game.RedCross borderType), cycle :: tail ) ->
            { cycle | redcross = Just time }
                :: tail
                |> beginBorder { borderType = borderType, time = time }

        ( Just (Game.Fixation borderType), cycle :: tail ) ->
            { cycle | fixation = Just time }
                :: tail
                |> beginBorder { borderType = borderType, time = time }

        ( Just (Game.Probe borderType direction), cycle :: tail ) ->
            { cycle
                | probe = Just time
                , probeIndex = Just (Game.directionToIndex direction)
            }
                :: tail
                |> beginBorder { borderType = borderType, time = time }


beginBorder : { borderType : Game.BorderType, time : Time } -> List Game.Cycle -> List Game.Cycle
beginBorder { borderType, time } cycles =
    case ( borderType, cycles ) of
        ( _, [] ) ->
            cycles

        ( Game.None, _ ) ->
            cycles

        ( Game.Blue, cycle :: tail ) ->
            if cycle.border == Nothing then
                { cycle | border = Just time, blue = True } :: tail
            else
                cycles

        ( Game.Grey, cycle :: tail ) ->
            if cycle.border == Nothing then
                { cycle | border = Just time, grey = True } :: tail
            else
                cycles

        ( Game.Black, cycle :: tail ) ->
            if cycle.border == Nothing then
                { cycle | border = Just time, blue = False, grey = False } :: tail
            else
                cycles

        ( Game.Dashed, cycle :: tail ) ->
            if cycle.border == Nothing then
                { cycle | border = Just time, dash = True } :: tail
            else
                cycles


timeout : Time -> List Game.Cycle -> List Game.Cycle
timeout time cycles =
    case cycles of
        [] ->
            cycles

        cycle :: tail ->
            { cycle | timeout = Just time } :: tail


acceptDirection : { desired : Game.Direction, actual : Game.Direction, time : Time } -> List Game.Cycle -> List Game.Cycle
acceptDirection { desired, actual, time } cycles =
    case cycles of
        [] ->
            cycles

        cycle :: tail ->
            { cycle | selection = Just time, selectedIndex = Game.directionToIndex actual } :: tail


acceptIndication : { desired : Bool, time : Time } -> List Game.Cycle -> List Game.Cycle
acceptIndication { desired, time } cycles =
    case cycles of
        [] ->
            cycles

        cycle :: tail ->
            cycles


acceptSelection : { desired : Int, actual : Int, time : Time } -> List Game.Cycle -> List Game.Cycle
acceptSelection { desired, actual, time } cycles =
    case cycles of
        [] ->
            cycles

        cycle :: tail ->
            cycles
