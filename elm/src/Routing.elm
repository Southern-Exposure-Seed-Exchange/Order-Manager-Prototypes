module Routing exposing (..)

import String
import Navigation
import UrlParser exposing (..)

import Api.Models exposing (CategoryId, ProductId)


type Route
    = DashboardRoute
    | CategoriesRoute
    | CategoryRoute CategoryId
    | ProductsRoute
    | ProductRoute ProductId
    | NotFoundRoute


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ format DashboardRoute (s "")
        , format DashboardRoute (s "dashboard")
        , format CategoryRoute (s "categories" </> int)
        , format CategoriesRoute (s "categories")
        , format ProductRoute (s "products" </> int)
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
