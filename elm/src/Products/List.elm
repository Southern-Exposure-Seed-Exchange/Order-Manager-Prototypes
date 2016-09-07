module Products.List exposing (..)

import Html exposing (..)

import Api.Models exposing (Product)
import Products.Messages exposing (Msg(..))
import Products.Models exposing (ProductData)

view : ProductData -> Html Msg
view model =
    div []
        [ h1 [] [ text "Products" ]
        , button [] [ text "New Product" ]
        , prodTable model.products
        ]


prodTable : List Product -> Html msg
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


prodRow : Product -> Html msg
prodRow product =
    tr []
        [ td [] [ text product.name ]
        , td [] [ text <| toString product.isOrganic ]
        , td [] [ text <| toString product.isHeirloom ]
        , td [] [ text <| toString product.isSouthEast ]
        , td [] [ text <| toString product.isActive ]
        ]
