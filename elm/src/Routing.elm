module Routing exposing (..)

{-| This module is responsible for defining the application's Routes, along
with helper functions like the URL parser.

# Definitions
@docs Route, matchers

# Helpers
@docs routeFromResult, matchers, hashParser, parser

-}

import String
import Navigation
import UrlParser exposing (..)
import Api.Models exposing (CategoryId, ProductId)


{-| The Route datatype describes all possible pages in the application.
-}
type Route
    = DashboardRoute
    | CategoriesRoute
    | CategoryAddRoute
    | CategoryRoute CategoryId
    | CategoryEditRoute CategoryId
    | ProductAddRoute
    | ProductsRoute
    | ProductRoute ProductId
    | ProductsEditRoute ProductId
    | NotFoundRoute


{-| A parser describing the URL schema for each `Route`.
-}
matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ format DashboardRoute (s "")
        , format DashboardRoute (s "dashboard")
        , format CategoryEditRoute (s "categories" </> int </> s "edit")
        , format CategoryRoute (s "categories" </> int)
        , format CategoryAddRoute (s "categories" </> s "add")
        , format CategoriesRoute (s "categories")
        , format ProductsEditRoute (s "products" </> int </> s "edit")
        , format ProductRoute (s "products" </> int)
        , format ProductAddRoute (s "products" </> s "add")
        , format ProductsRoute (s "products")
        ]


{-| Build upon the `matchers` function to parse locations specified by URL
hashes.
-}
hashParser : Navigation.Location -> Result String Route
hashParser location =
    location.hash
        |> String.dropLeft 1
        |> parse identity matchers


{-| Create a `Navigation.Parser` using the `hashParser` function.
-}
parser : Navigation.Parser (Result String Route)
parser =
    Navigation.makeParser hashParser


{-| Pull a Route out of a `Result`, falling back to the `NotFoundRoute`
if an error occured.
-}
routeFromResult : Result String Route -> Route
routeFromResult result =
    case result of
        Ok route ->
            route

        Err _ ->
            NotFoundRoute
