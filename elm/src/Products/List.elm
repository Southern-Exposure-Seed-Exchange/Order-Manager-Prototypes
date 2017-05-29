module Products.List exposing (..)

import Dict
import Html exposing (..)
import Html.Attributes exposing (colspan, hidden, class, href)
import Html.Events exposing (onClick)
import Api.Models exposing (Product, ProductVariant)
import Products.Messages exposing (Msg(..))
import Products.Models exposing (ProductData)
import Utils exposing (filterById, onClickNoDefault, toDollars)


view : ProductData -> Html Msg
view model =
    div []
        [ h1 [] [ text "Products" ]
        , button [ class "btn-sm", onClick AddProduct ] [ text "New Product" ]
        , text " "
        , button [ onClick ToggleAllSKUs, class "btn-sm" ] [ text "Toggle SKUs" ]
        , prodTable model
        ]


prodTable : ProductData -> Html Msg
prodTable model =
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
        , tbody [] (List.concatMap (prodRow model) model.products)
        ]


prodRow : ProductData -> Product -> List (Html Msg)
prodRow model product =
    let
        showSKUs =
            Dict.get product.id model.showSKUs
                |> Maybe.withDefault False

        variants =
            filterById .product product model.productVariants

        extraRows =
            [ tr [ hidden <| not showSKUs, class "variant-header" ]
                [ th [ colspan 2, class "hide-border" ] []
                , th [] [ text "SKU" ]
                , th [] [ text "Weight" ]
                , th [] [ text "Price" ]
                ]
            ]
                ++ List.map (skuRow showSKUs) variants
    in
        [ tr [ onClick (ToggleSKUs product.id) ]
            [ td []
                [ a
                    [ onClickNoDefault <| VisitProduct product.id
                    , href <| "#products/" ++ toString product.id
                    ]
                    [ text product.name ]
                ]
            , td [] [ text <| toString product.isOrganic ]
            , td [] [ text <| toString product.isHeirloom ]
            , td [] [ text <| toString product.isSouthEast ]
            , td [] [ text <| toString product.isActive ]
            ]
        ]
            ++ extraRows


skuRow : Bool -> ProductVariant -> Html Msg
skuRow show productVariant =
    tr [ hidden <| not show, class "variant-row" ]
        [ td [ colspan 2, class "hide-border" ] []
        , td [] [ text productVariant.sku ]
        , td [] [ text <| toString productVariant.weight ]
        , td [] [ text <| "$" ++ toDollars productVariant.price ]
        ]
