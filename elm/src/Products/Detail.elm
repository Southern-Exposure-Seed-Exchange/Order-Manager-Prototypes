module Products.Detail exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href, class)
import Html.Events exposing (onClick)
import Api.Models exposing (Product, ProductVariant)
import Products.Models exposing (ProductData)
import Products.Messages exposing (Msg(..))
import Utils exposing (getById, filterBy, onClickNoDefault)


view : Product -> ProductData -> Html Msg
view product model =
    let
        maybeCategory =
            getById model.categories product.category

        productVariants =
            filterBy .product product.id model.productVariants

        categoryLink category =
            small []
                [ a
                    [ onClickNoDefault (VisitCategory category.id)
                    , href <| "#categories/" ++ toString category.id
                    ]
                    [ text <| "(" ++ category.name ++ ")" ]
                ]
    in
        div []
            [ h1 [] [ text "Products" ]
            , h2 []
                [ text <| product.name ++ " "
                , Maybe.map categoryLink maybeCategory |> Maybe.withDefault (text "")
                ]
            , p []
                [ button [ class "btn-sm", onClick (EditProduct product.id) ]
                    [ text "Edit Product" ]
                , text " "
                , button [ class "btn-sm danger", onClick (DeleteProduct product.id) ]
                    [ text "Delete" ]
                ]
            , p [] [ text product.description ]
            , h4 [] [ text "Product Variants" ]
            , variantTable productVariants
            ]


variantTable : List ProductVariant -> Html Msg
variantTable productVariants =
    table []
        [ thead []
            [ tr [ class "variant-header" ]
                [ th [] [ text "SKU" ]
                , th [] [ text "Weight" ]
                , th [] [ text "Price" ]
                ]
            ]
        , tbody [] <| List.map variantRow productVariants
        ]


variantRow : ProductVariant -> Html Msg
variantRow productVariant =
    tr [ class "variant-row" ]
        [ td [] [ text productVariant.sku ]
        , td [] [ text <| toString productVariant.weight ]
        , td [] [ text <| toString productVariant.price ]
        ]
