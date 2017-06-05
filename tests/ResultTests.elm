module ResultTests exposing (all)

import Test exposing (..)
import Result.GoNoGoTests
import Result.DotProbeTests
import Result.StopSignalTests
import Result.VisualSearchTests


all : Test
all =
    describe "Cycle Test Suite"
        [ Result.GoNoGoTests.all
        , Result.DotProbeTests.all
        , Result.StopSignalTests.all
        , Result.VisualSearchTests.all
        ]
