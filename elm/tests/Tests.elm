module Tests exposing (..)

import Test exposing (Test, describe)
import Tests.Routing as RoutingTests
import Tests.Utils as UtilTests


all : Test
all =
    describe "All Tests"
        [ RoutingTests.tests
        , UtilTests.tests
        ]
