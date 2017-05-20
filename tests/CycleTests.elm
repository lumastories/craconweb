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
                          , targetIndex = 1
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
            , test "Correct Go" <|
                \() ->
                    Expect.equal
                        [ { id = Nothing
                          , sessionId = "sessionId"
                          , sort = 0
                          , fixation = Nothing
                          , selection = Just 1495240100609
                          , pictures = Just 1495240100139
                          , redcross = Nothing
                          , probe = Nothing
                          , border = Just 1495240100139
                          , timeout = Nothing
                          , rest = Just 1495240100609
                          , width = Nothing
                          , height = Nothing
                          , blue = False
                          , grey = False
                          , dash = False
                          , probeIndex = Nothing
                          , targetIndex = 0
                          , selectedIndex = 0
                          , startIndex = 0
                          , images = [ "2543813863820289509" ]
                          }
                        ]
                        (Game.Cycle.generate "sessionId"
                            [ EndTrial 1495240100609
                            , AcceptDirection { desired = Left, actual = Left } 1495240100609
                            , BeginInput 1495240100139
                            , BeginDisplay (Just (LeftOrRight Black Left { url = "http://localhost:8654/repo/e585413c320e34313f1bed18dba2945a.png", id = "2543813863820289509" })) 1495240100139
                            , BeginTrial 1495240100139
                            ]
                        )
            , test "Incorrect Go" <|
                \() ->
                    Expect.equal
                        [ { id = Nothing
                          , sessionId = "sessionId"
                          , sort = 0
                          , fixation = Nothing
                          , selection = Just 1495248757557
                          , pictures = Just 1495248756701
                          , redcross = Just 1495248757557
                          , probe = Nothing
                          , border = Just 1495248756701
                          , timeout = Nothing
                          , rest = Just 1495248758455
                          , width = Nothing
                          , height = Nothing
                          , blue = False
                          , grey = False
                          , dash = False
                          , probeIndex = Nothing
                          , targetIndex = 1
                          , selectedIndex = 0
                          , startIndex = 1
                          , images = [ "755506963820219512" ]
                          }
                        ]
                        (Game.Cycle.generate "sessionId"
                            [ EndTrial 1495248758455
                            , BeginDisplay (Just (RedCross Black)) 1495248757557
                            , AcceptDirection { desired = Right, actual = Left } 1495248757557
                            , BeginInput 1495248756701
                            , BeginDisplay (Just (LeftOrRight Black Right { url = "http://localhost:8654/repo/e310af4d3a76f05b81b1e41e0921e8c5.png", id = "755506963820219512" })) 1495248756701
                            , BeginTrial 1495248756701
                            ]
                        )
            ]
        ]
