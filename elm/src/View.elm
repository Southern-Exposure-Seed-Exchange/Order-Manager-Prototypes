module View exposing (..)

import Html exposing (..)
import Html.App
import Html.Events exposing (onClick)

import Api.Models exposing (CategoryId)
import Categories.Detail
import Categories.List
import Messages exposing (Msg(..))
import Models exposing (Model)
import Products.List
import Products.Models exposing (makeProductData)
import Routing exposing (Route(..))


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
        , a [ onClick (RoutingMsg ProductsRoute) ]
            [ text "Products" ]
        ]


page : Model -> Html Msg
page model =
    case model.route of
        DashboardRoute ->
            dashboard
        CategoriesRoute ->
            { categories = model.categories
            , products = model.products }
                |> Categories.List.view
                |> Html.App.map CategoriesMsg
        CategoryRoute categoryId ->
            maybeCategoryView model categoryId
        ProductsRoute ->
            makeProductData model
                |> Products.List.view
                |> Html.App.map ProductsMsg
        NotFoundRoute ->
            notFound


maybeCategoryView : Model -> CategoryId -> Html Msg
maybeCategoryView model categoryId =
    let
        category =
            model.categories
                |> List.filter (\c -> c.id == categoryId)
                |> List.head
    in
       case category of
           Nothing ->
               notFound
           Just cat ->
                Categories.Detail.view cat { categories = model.categories, products = model.products }
                    |> Html.App.map CategoriesMsg


notFound : Html Msg
notFound =
    div [] [h1 [] [ text "404 - Page Not Found" ] ]


dashboard : Html Msg
dashboard =
    div []
        [ h1 [] [ text "Dashboard" ]
        , p [] [ text "At some point there'll be cool stuff here." ]
        , p [] [ text "Backends should implement:" ]
        , ul []
            [ li [] [ text "Categories - list, view" ]
            , li [] [ text "Products - list" ]
            ]
        ]
