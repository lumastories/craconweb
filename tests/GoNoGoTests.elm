module GoNoGoTests exposing (all)

import Test exposing (..)
import Empty exposing (initialModel)
import Expect
import Update
import Model exposing (Msg(..))
import Date
import Entity
import Protobuf
import Time
import Game
import Random
import Game.Implementations.GoNoGo as GoNoGo
import Game.Cycle as Cycle


all : Test
all =
    describe "Go/No Go Game"
        [ singleCycle ]


singleCycle : Test
singleCycle =
    test "Single Cycle" <|
        \() ->
            let
                msgs =
                    [ InitGoNoGo
                    , StartSession
                        { gameId = game.id
                        , game = gameStateData
                        , time = timestamp
                        , initialSeed = (round timestamp)
                        , nextSeed = nextSeed
                        }
                    , NewCurrentTime (timestamp + (10 * Time.millisecond))
                    , DirectionInput Game.Left

                    -- , NewCurrentTime (timestamp + (1250 * Time.millisecond))
                    -- , NewCurrentTime (timestamp + (1262 * Time.millisecond))
                    ]
            in
                Expect.equal
                    ([ { id = Nothing
                       , sessionId = "SessionId"
                       , sort = 0
                       , fixation = Nothing
                       , selection = Just 0
                       , pictures = Just 0
                       , redcross = Just 0
                       , probe = Nothing
                       , border = Just 0
                       , timeout = Nothing
                       , rest = Nothing
                       , interval = Nothing
                       , width = Just 2
                       , height = Nothing
                       , blue = False
                       , gray = False
                       , dash = True
                       , targetIndex = 0
                       , selectedIndex = 0
                       , startIndex = 0
                       , images = [ "non-response" ]
                       }
                     ]
                    )
                    ((List.foldl (\msg model -> Update.update msg model |> Tuple.first |> debug) goNoGoModel msgs) |> toCycles)


toLogEntries : Model.Model -> List Game.LogEntry
toLogEntries model =
    model
        |> toState
        |> Maybe.map .log
        |> Maybe.withDefault []


toCycles : Model.Model -> List Game.Cycle
toCycles model =
    model
        |> toLogEntries
        |> Cycle.generate "SessionId"


toState : Model.Model -> Maybe Game.State
toState model =
    case model.gameState of
        Game.Playing { game } ->
            Just (Game.unwrap game)

        _ ->
            Nothing


timestamp : Time.Time
timestamp =
    0


protobufTimestamp : Protobuf.Timestamp
protobufTimestamp =
    Date.fromTime timestamp


game : Entity.Game
game =
    { id = "TEST"
    , name = "Go No-Go"
    , slug = "gonogo"
    , dscript = ""
    , instruct = ""
    , icon = "gonogo.png"
    , reactDur = 500
    , sessDur = 300000
    , trialDur = 1250
    , offsetDur = 0
    , fixDur = 500
    , fixImg = "x.png"
    , durInc = 0
    , durDec = 0
    , incTrigger = 0
    , decTrigger = 0
    , blocked = Nothing
    , created = Just protobufTimestamp
    , updated = Just protobufTimestamp
    , deleted = Nothing
    }


initialSeed : Random.Seed
initialSeed =
    Random.initialSeed (round timestamp)


responseImage : Game.Image
responseImage =
    { id = "response"
    , url = "response"
    }


nonResponseImage : Game.Image
nonResponseImage =
    { id = "non-response"
    , url = "non-response"
    }


fillerImage : Game.Image
fillerImage =
    { id = "fillter"
    , url = "fillter"
    }


( gameStateData, nextSeed ) =
    GoNoGo.init
        { totalDuration = 1250 * Time.millisecond
        , infoString = """
<h3 class="title">Instructions</h3>
<p>You will see pictures either on the left or right side of the screen, surrounded by a solid or dashed border. Press <span class="highlight"><strong>c</strong></span> when the picture is on the left side of the screen or <span class="highlight"><strong>m</strong></span> when the picture is on the right side of the screen. BUT only if you see a <span style="border: 1px solid rgb(0, 0, 0); padding: 2px;">solid border</span> around the picture. Do not press if you see a <span style="border: 1px dashed rgb(0, 0, 0); padding: 2px;">dashed border</span>. Go as fast as you can, but don't sacrifice accuracy for speed.<div>
<br>
<br>
<strong>Press any key or tap here to continue.</strong></div>
</p>
"""
        , responseImages = List.range 1 10 |> List.map (always responseImage)
        , nonResponseImages = List.range 1 10 |> List.map (always nonResponseImage)
        , fillerImages = List.range 1 10 |> List.map (always fillerImage)
        , seedInt = (round timestamp)
        , currentTime = timestamp
        , redCrossDuration = 500 * Time.millisecond
        , blockDuration = 1 * Time.minute
        , totalBlocks = 5
        , restDuration = 10 * Time.second
        , intervalMin = 500 * Time.millisecond
        , intervalJitter = 0
        }


gameSession : Game.Session
gameSession =
    { id = ""
    , userId = ""
    , gameId = ""
    , seed = (round timestamp)
    , start = timestamp
    , end = Nothing
    , jitter = False
    }


goNoGoModel : Model.Model
goNoGoModel =
    { initialModel
        | gonogoGame = Just game
        , gameState =
            Game.Playing
                { game = gameStateData
                , session = gameSession
                , nextSeed = initialSeed
                }
    }


debug : Model.Model -> Model.Model
debug model =
    let
        -- _ =
        --     Debug.log "LogEntry" (model |> toLogEntries)
        -- _ =
        --     Debug.log "Cycles" (model |> toCycles)
        _ =
            Debug.log "time" (model |> toState |> Maybe.map .currTime)
    in
        model
