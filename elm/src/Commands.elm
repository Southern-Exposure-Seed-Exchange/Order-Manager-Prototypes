port module Commands exposing (..)

import Categories.Commands
import Messages exposing (Msg(..))
import Routing exposing (Route(..))
import Products.Commands


port setTitle : String -> Cmd msg


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

        ProductAddRoute ->
            Cmd.map CategoriesMsg Categories.Commands.fetchAll

        ProductsEditRoute id ->
            Cmd.batch
                [ Cmd.map CategoriesMsg (Categories.Commands.fetchAll)
                , Cmd.map ProductsMsg (Products.Commands.fetchOne id)
                ]

        ProductRoute id ->
            Cmd.map ProductsMsg (Products.Commands.fetchOne id)

        NotFoundRoute ->
            Cmd.none


setPageTitle : Route -> Cmd msg
setPageTitle route =
    setTitle <|
        case route of
            DashboardRoute ->
                "Dashboard"

            CategoriesRoute ->
                "Categories"

            CategoryAddRoute ->
                "Add Category"

            CategoryRoute _ ->
                "View Category"

            CategoryEditRoute _ ->
                "Edit Category"

            ProductsRoute ->
                "Products"

            ProductAddRoute ->
                "Add Product"

            ProductRoute _ ->
                "View Product"

            ProductsEditRoute _ ->
                "Edit Product"

            NotFoundRoute ->
                "404 Not Found"
