module View exposing (..)

import Html exposing (..)
import Html.App
import Html.Events exposing (onClick)

import Categories.Models exposing (makeCategoryData)
import Categories.Add
import Categories.Detail
import Categories.Edit
import Categories.List
import Messages exposing (Msg(..))
import Models exposing (Model)
import Products.Detail
import Products.List
import Products.Models exposing (makeProductData)
import Routing exposing (Route(..))
import Utils exposing (getById)


view : Model -> Html Msg
view model =
    div []
        [ nav
        , page model ]


nav : Html Msg
nav =
    div []
        [ span [] [text "Menu:  "]
        , a [ onClick (RoutingMsg DashboardRoute) ]
            [ text "Dashboard" ]
        , text " "
        , a [ onClick (RoutingMsg CategoriesRoute) ]
            [ text "Categories" ]
        , text " "
        , a [ onClick (RoutingMsg CategoryAddRoute) ]
            [ text "Add Category" ]
        , text " "
        , a [ onClick (RoutingMsg ProductsRoute) ]
            [ text "Products" ]
        ]


page : Model -> Html Msg
page model =
    case model.route of
        DashboardRoute ->
            dashboard
        CategoriesRoute ->
            makeCategoryData model
                |> Categories.List.view
                |> Html.App.map CategoriesMsg
        CategoryAddRoute ->
            makeCategoryData model
                |> Categories.Add.view
                |> Html.App.map CategoriesMsg
        CategoryRoute categoryId ->
            itemViewOr404 categoryId model.categories (makeCategoryData model)
                Categories.Detail.view CategoriesMsg
        CategoryEditRoute categoryId ->
            itemViewOr404 categoryId model.categories (makeCategoryData model)
                Categories.Edit.view CategoriesMsg
        ProductsRoute ->
            makeProductData model
                |> Products.List.view
                |> Html.App.map ProductsMsg
        ProductRoute productId ->
            itemViewOr404 productId model.products
                (makeProductData model) Products.Detail.view ProductsMsg
        NotFoundRoute ->
            notFound


itemViewOr404 : b -> List { a | id : b } -> c ->
                ({ a | id : b } -> c -> Html msg) -> (msg -> Msg) -> Html Msg
itemViewOr404 id items viewData view msg =
    case getById items id of
        Nothing ->
            notFound
        Just item ->
            view item viewData |> Html.App.map msg


notFound : Html Msg
notFound =
    div [] [h1 [] [ text "404 - Page Not Found" ] ]


dashboard : Html Msg
dashboard =
    div []
        [ h1 [] [ text "Dashboard" ]
        , p [] [ text "At some point there'll be cool stuff here." ]
        , ul []
            [ li [] [ text "Order Status Counts - New, Needs Packing, Awaiting Shipping, Back Ordered" ]
            , li [] [ text "New Orders Graph - Default to last 24 hours, allow date changes" ]
            , li [] [ text "Top Products Table - Most sold in XXX days/hours" ]
            , li [] [ text "Needs Packing Table - SKU, Name, Number of backordered orders/packets" ]
            ]
        , p [] [ text "Backends should implement:" ]
        , ul []
            [ li [] [ text "Categories - list, view" ]
            , li [] [ text "Products - list, view" ]
            ]
        ]
