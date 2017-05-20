module CycleTests exposing (all)

import Test exposing (..)
import Cycle.GoNoGoTests
import Cycle.DotProbeTests


all : Test
all =
    describe "Cycle Test Suite"
        [ Cycle.GoNoGoTests.all
        , Cycle.DotProbeTests.all
        ]
