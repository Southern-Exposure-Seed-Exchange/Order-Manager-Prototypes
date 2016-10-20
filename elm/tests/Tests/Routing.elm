module Tests.Routing exposing (tests)

import Test exposing (Test, describe, test)
import Routing exposing (hashParser, routeFromResult, Route(..))
import Navigation
import Expect


tests : Test
tests =
    let
        parse =
            hashParser >> routeFromResult

        parserTest ( route, url ) =
            test (toString route ++ " should match #" ++ url) <|
                \_ -> Expect.equal (parse { mockLocation | hash = url }) route
    in
        describe "Routing.hashParser" <|
            List.map parserTest
                [ ( DashboardRoute, "/" )
                , ( DashboardRoute, "/dashboard" )
                , ( CategoriesRoute, "/categories" )
                , ( CategoryAddRoute, "/categories/add" )
                , ( CategoryRoute 42, "/categories/42" )
                , ( CategoryEditRoute 42, "/categories/42/edit" )
                , ( ProductsRoute, "/products" )
                , ( ProductAddRoute, "/products/add" )
                , ( ProductRoute 42, "/products/42" )
                , ( ProductsEditRoute 42, "/products/42/edit" )
                , ( NotFoundRoute, "/blahblahgiberishnotexists" )
                ]



-- Mock Navigation.Locations


dontCare : String
dontCare =
    "Don't Care"


mockLocation : Navigation.Location
mockLocation =
    { href = dontCare
    , host = dontCare
    , hostname = dontCare
    , protocol = dontCare
    , origin = dontCare
    , port_ = dontCare
    , pathname = dontCare
    , search = dontCare
    , hash = dontCare
    , username = dontCare
    , password = dontCare
    }
