module Tests exposing (..)

import Test exposing (Test, describe)
import Tests.Categories as CategoryTests
import Tests.Routing as RoutingTests
import Tests.Utils as UtilTests


all : Test
all =
    describe "All Tests"
        [ CategoryTests.tests
        , RoutingTests.tests
        , UtilTests.tests
        ]
