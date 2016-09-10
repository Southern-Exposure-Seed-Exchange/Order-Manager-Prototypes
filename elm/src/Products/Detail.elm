module Products.Detail exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)

import Api.Models exposing (Product, ProductVariant)
import Products.Models exposing (ProductData)
import Products.Messages exposing (Msg(..))
import Utils exposing (getById, filterBy)


view : Product -> ProductData -> Html Msg
view product model =
    let
        maybeCategory =
            getById model.categories product.category
        productVariants =
            filterBy .product product.id model.productVariants
        categoryLink category =
            small []
                [ a [ onClick (VisitCategory category.id) ]
                    [ text <| "(" ++ category.name ++ ")" ] ]
    in
        div []
            [ h1 [] [ text "Products" ]
            , h2 []
                [ text <| product.name ++ " "
                , Maybe.map categoryLink maybeCategory |> Maybe.withDefault (text "")
                ]
            , p [] [ button [] [ text "Edit Product" ], button [] [ text "Delete" ] ]
            , p [] [ text product.description ]
            , h4 [] [ text "Product Variants" ]
            , variantTable productVariants
            ]


variantTable : List ProductVariant -> Html Msg
variantTable productVariants =
    table []
        [ thead []
            [ tr []
                [ th [] [ text "SKU" ]
                , th [] [ text "Weight" ]
                , th [] [ text "Price" ]
                ]
            ]
        , tbody [] <| List.map variantRow productVariants
        ]


variantRow : ProductVariant -> Html Msg
variantRow productVariant =
    tr []
        [ td [] [ text productVariant.sku ]
        , td [] [ text <| toString productVariant.weight ]
        , td [] [ text <| toString productVariant.price ]
        ]
