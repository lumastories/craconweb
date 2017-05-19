module CycleTests exposing (all)

import Test exposing (..)
import Expect
import Game exposing (LogEntry(..), BorderType(..), Layout(..), Direction(..))
import Game.Cycle


all : Test
all =
    describe "Cycle Test Suite"
        [ describe "GoNoGo"
            [ test "Timeout Go" <|
                \() ->
                    Expect.equal
                        [ { id = Nothing
                          , sessionId = "sessionId"
                          , sort = 0
                          , fixation = Nothing
                          , selection = Nothing
                          , pictures = Just 1495162244273
                          , redcross = Just 1495162245538
                          , probe = Nothing
                          , border = Just 1495162244273
                          , timeout = Just 1495162245538
                          , rest = Just 1495162246038
                          , width = Nothing
                          , height = Nothing
                          , blue = False
                          , grey = False
                          , dash = False
                          , probeIndex = Nothing
                          , targetIndex = 0
                          , selectedIndex = 0
                          , startIndex = 1
                          , images = [ "6920533513820449510" ]
                          }
                        ]
                        (Game.Cycle.generate "sessionId"
                            [ EndTrial 1495162246038
                            , BeginDisplay (Just (RedCross Black)) 1495162245538
                            , Timeout { desired = False } 1495162245538
                            , BeginInput 1495162244273
                            , BeginDisplay (Just (LeftOrRight Black Right { url = "http://localhost:8654/repo/36ba69efa7c71b0430cb0a5ecfaff5be.png", id = "6920533513820449510" })) 1495162244273
                            , BeginTrial 1495162244273
                            ]
                        )
            ]
        ]
