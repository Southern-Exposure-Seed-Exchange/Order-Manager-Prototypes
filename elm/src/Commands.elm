module Commands exposing (..)

import Categories.Commands
import Messages exposing (Msg(..))
import Routing exposing (Route(..))
import Products.Commands


fetchForRoute : Route -> Cmd Msg
fetchForRoute route =
    case route of
        DashboardRoute ->
            Cmd.none
        CategoriesRoute ->
            Cmd.map CategoriesMsg Categories.Commands.fetchAll
        CategoryRoute id ->
            Cmd.map CategoriesMsg (Categories.Commands.fetchOne id)
        ProductsRoute ->
            Cmd.map ProductsMsg Products.Commands.fetchAll
        NotFoundRoute ->
            Cmd.none
