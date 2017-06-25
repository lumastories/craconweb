module CycleTests exposing (all)

import Test exposing (..)
import Cycle.GoNoGoTests
import Cycle.DotProbeTests
import Cycle.StopSignalTests
import Cycle.FmriStopSignalTests
import Cycle.VisualSearchTests


all : Test
all =
    describe "Cycle Test Suite"
        [ Cycle.GoNoGoTests.all
        , Cycle.DotProbeTests.all
        , Cycle.StopSignalTests.all
        , Cycle.VisualSearchTests.all
        , Cycle.FmriStopSignalTests.all
        ]
