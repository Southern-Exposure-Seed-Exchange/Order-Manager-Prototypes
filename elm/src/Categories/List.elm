module Categories.List exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Maybe

import Categories.Messages exposing (..)
import Categories.Models exposing (CategoryData, Category, Product)


view : CategoryData -> Html Msg
view model =
    let
        isNothing maybe =
            case maybe of
                Nothing ->
                    True
                Just _ ->
                    False
        rootCategories =
            List.filter (\cat -> isNothing cat.parent) model.categories
    in
        div []
            [ h1 [] [ text "Categories" ]
            , button [] [ text "New Category" ]
            , catTable model rootCategories ]


catTable : CategoryData -> List Category -> Html Msg
catTable model categories =
        table []
            [ thead []
                [ tr []
                    [ th [] [ text "Name" ]
                    , th [] [ text "Categories" ]
                    , th [] [ text "Products" ]
                    ]
                ]
            , tbody [] (List.map (catRow model) categories)
            ]


catRow : CategoryData -> Category -> Html Msg
catRow model category =
    tr [ onClick <| VisitCategory category.id ]
        [ td [] [ text category.name ]
        , td [] [ text (childCount model.categories category |> toString) ]
        , td [] [ text (productCount model.products category |> toString) ]
        ]


childCount : List Category -> Category -> Int
childCount categories category =
    List.filter (\c -> Maybe.withDefault 0 c.parent == category.id) categories
        |> List.length


productCount : List Product -> Category -> Int
productCount products category =
    List.filter (\p -> p.category == category.id) products
        |> List.length
