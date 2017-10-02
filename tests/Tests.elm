module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String


-- import CycleTests
-- import ResultTests

import GoNoGoTests


all : Test
all =
    describe "Test Suite"
        [ --CycleTests.all
          --, ResultTests.all
          GoNoGoTests.all
        ]
