module Categories.Detail exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)

import Api.Models exposing (Category, Product, CategoryId)
import Categories.List exposing (catTable)
import Categories.Messages exposing (Msg(..))
import Categories.Models exposing (CategoryData)


view : Category -> CategoryData -> Html Msg
view category model =
    let
        isChildCategory otherCategory =
            case otherCategory.parent of
                Nothing ->
                    False
                Just parentId ->
                    category.id == parentId
        subCategories =
            List.filter isChildCategory model.categories
        products =
            List.filter (\p -> p.category == category.id) model.products
        parentCategory =
            category.parent `Maybe.andThen`
                (\parentId -> List.filter (\c -> parentId == c.id) model.categories |> List.head)
        parentLink =
            case parentCategory of
                Nothing ->
                    text ""
                Just parent ->
                    small []
                        [ a [ onClick <| VisitCategory parent.id ]
                            [ text <| " (" ++ parent.name ++ ")" ]
                        ]
        categoryTable =
            if List.isEmpty subCategories
                then text ""
                else div [] [ h4 [] [ text "Categories" ]
                            , catTable model subCategories ]
        productsTable =
            if List.isEmpty products
               then text "This Category has no Products."
               else prodTable products
    in
        div []
        [ h1 [] [ text "Categories" ]
        , h2 [] [ text category.name, parentLink ]
        , p [] [ text category.description ]
        , p []
            [ button [] [ text "Edit Category" ]
            , button [] [ text "Delete" ]
            ]
        , categoryTable
        , div [] [ h4 [] [ text "Products" ], productsTable ]
        ]

prodTable : List Product -> Html Msg
prodTable products =
    table []
        [ thead []
            [ tr []
                [ th [] [ text "Name" ]
                , th [] [ text "Organic" ]
                , th [] [ text "Heirloom" ]
                , th [] [ text "SouthEast" ]
                , th [] [ text "Active" ]
                ]
            ]
        , tbody [] (List.map prodRow products)
        ]


prodRow : Product -> Html Msg
prodRow product =
    tr []
        [ td [] [ text product.name ]
        , td [] [ text <| toString product.isOrganic ]
        , td [] [ text <| toString product.isHeirloom ]
        , td [] [ text <| toString product.isSouthEast ]
        , td [] [ text <| toString product.isActive ]
        ]
