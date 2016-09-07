module Routing exposing (..)

import String
import Navigation
import UrlParser exposing (..)

import Api.Models exposing (CategoryId)


type Route
    = DashboardRoute
    | CategoryRoute CategoryId
    | CategoriesRoute
    | ProductsRoute
    | NotFoundRoute


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ format DashboardRoute (s "")
        , format DashboardRoute (s "dashboard")
        , format CategoryRoute (s "categories" </> int)
        , format CategoriesRoute (s "categories")
        , format ProductsRoute (s "products")
        ]


hashParser : Navigation.Location -> Result String Route
hashParser location =
    location.hash
        |> String.dropLeft 1
        |> parse identity matchers


parser : Navigation.Parser (Result String Route)
parser =
    Navigation.makeParser hashParser


routeFromResult : Result String Route -> Route
routeFromResult result =
    case result of
        Ok route ->
            route
        Err _ ->
            NotFoundRoute
