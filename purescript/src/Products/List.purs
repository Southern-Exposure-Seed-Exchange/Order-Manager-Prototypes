module Products.List where

import Prelude hiding (div)
import Data.Array (fromFoldable)
import Data.List (List)
import Pux.Html (Html, div, h1, text, button, table, thead, tbody, tr, th, td)

import Api.Models (Product(..))
import Products.Models (ProductData(..))
import Products.Messages (Msg)


view :: ProductData -> Html Msg
view (ProductData model) =
    div []
        [ h1 [] [ text "Products" ]
        , button [] [ text "New Product" ]
        , button [] [ text "Toggle SKUs" ]
        , productsTable model.products
        ]


productsTable :: forall msg. List Product -> Html msg
productsTable products =
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
        , tbody [] <<< fromFoldable $ map prodRow products
        ]


prodRow :: forall msg. Product -> Html msg
prodRow (Product product) =
    tr []
        [ td [] [ text product.name ]
        , td [ ] [ text $ show product.isOrganic ]
        , td [ ] [ text $ show product.isHeirloom ]
        , td [ ] [ text $ show product.isSouthEast ]
        , td [ ] [ text $ show product.isActive ]
        ]
