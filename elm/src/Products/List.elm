module Products.List exposing (..)

import Dict
import Html exposing (..)
import Html.Attributes exposing (colspan, hidden)
import Html.Events exposing (onClick)
import Api.Models exposing (Product, ProductVariant)
import Products.Messages exposing (Msg(..))
import Products.Models exposing (ProductData)
import Utils exposing (filterBy)


view : ProductData -> Html Msg
view model =
    div []
        [ h1 [] [ text "Products" ]
        , button [] [ text "New Product" ]
        , button [ onClick ToggleAllSKUs ] [ text "Toggle SKUs" ]
        , prodTable model
        ]


prodTable : ProductData -> Html Msg
prodTable model =
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
        , tbody [] (List.concatMap (prodRow model) model.products)
        ]


prodRow : ProductData -> Product -> List (Html Msg)
prodRow model product =
    let
        showSKUs =
            Dict.get product.id model.showSKUs
                |> Maybe.withDefault False

        variants =
            filterBy .product product.id model.productVariants

        extraRows =
            [ tr [ hidden <| not showSKUs ]
                [ th [ colspan 2 ] []
                , th [] [ text "SKU" ]
                , th [] [ text "Weight" ]
                , th [] [ text "Price" ]
                ]
            ]
                ++ List.map (skuRow showSKUs) variants
    in
        [ tr [ onClick (ToggleSKUs product.id) ]
            [ td [ onClick (VisitProduct product.id) ] [ text product.name ]
            , td [] [ text <| toString product.isOrganic ]
            , td [] [ text <| toString product.isHeirloom ]
            , td [] [ text <| toString product.isSouthEast ]
            , td [] [ text <| toString product.isActive ]
            ]
        ]
            ++ extraRows


skuRow : Bool -> ProductVariant -> Html Msg
skuRow show productVariant =
    tr [ hidden <| not show ]
        [ td [ colspan 2 ] []
        , td [] [ text productVariant.sku ]
        , td [] [ text <| toString productVariant.weight ]
        , td [] [ text <| toString productVariant.price ]
        ]
