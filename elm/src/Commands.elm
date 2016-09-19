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

        CategoryAddRoute ->
            Cmd.map CategoriesMsg Categories.Commands.fetchAll

        CategoryRoute id ->
            Cmd.map CategoriesMsg (Categories.Commands.fetchOne id)

        CategoryEditRoute id ->
            Cmd.batch
                [ Cmd.map CategoriesMsg (Categories.Commands.fetchOne id)
                , Cmd.map CategoriesMsg (Categories.Commands.fetchAll)
                ]

        ProductsRoute ->
            Cmd.map ProductsMsg Products.Commands.fetchAll

        ProductRoute id ->
            Cmd.map ProductsMsg (Products.Commands.fetchOne id)

        NotFoundRoute ->
            Cmd.none
