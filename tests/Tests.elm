module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import CycleTests


all : Test
all =
    describe "Test Suite"
        [ CycleTests.all
        ]
