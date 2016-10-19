module Categories.Detail exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href, class)
import Html.Events exposing (onClick)
import Api.Models exposing (Category, Product, CategoryId)
import Categories.List exposing (catTable)
import Categories.Messages exposing (Msg(..))
import Categories.Models exposing (CategoryData)
import Utils exposing (getById, filterBy, onClickNoDefault)


view : Category -> CategoryData -> Html Msg
view category model =
    let
        subCategories =
            filterBy (.parent >> Maybe.withDefault 0) category.id model.categories

        products =
            filterBy .category category.id model.products

        parentCategory =
            category.parent `Maybe.andThen` getById model.categories

        parentLink =
            case parentCategory of
                Nothing ->
                    text ""

                Just parent ->
                    small []
                        [ text " "
                        , a
                            [ onClickNoDefault <| VisitCategory parent.id
                            , href <| "#categories/" ++ toString parent.id
                            ]
                            [ text <| "(" ++ parent.name ++ ")" ]
                        ]

        categoryTable =
            if List.isEmpty subCategories then
                text ""
            else
                div []
                    [ h4 [] [ text "Categories" ]
                    , catTable model subCategories
                    ]

        productsTable =
            if List.isEmpty products then
                text "This Category has no Products."
            else
                prodTable products
    in
        div []
            [ h1 [] [ text "Categories" ]
            , h2 [] [ text category.name, parentLink ]
            , p [] [ text category.description ]
            , p []
                [ button [ onClick <| EditCategory category.id, class "btn-sm" ]
                    [ text "Edit Category" ]
                , text " "
                , button [ onClick <| DeleteCategory category.id, class "btn-sm danger" ]
                    [ text "Delete Category" ]
                ]
            , categoryTable
            , div [] [ h4 [] [ text "Products" ], productsTable ]
            ]


prodTable : List Product -> Html Msg
prodTable products =
    table [ class "table-condensed" ]
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
        [ td []
            [ a
                [ onClickNoDefault <| VisitProduct product.id
                , href <| "#/products/" ++ toString product.id
                ]
                [ text product.name ]
            ]
        , td [] [ text <| toString product.isOrganic ]
        , td [] [ text <| toString product.isHeirloom ]
        , td [] [ text <| toString product.isSouthEast ]
        , td [] [ text <| toString product.isActive ]
        ]
