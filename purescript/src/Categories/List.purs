module Categories.List where

import Prelude hiding (div)
import Data.Array (fromFoldable)
import Data.List (List, length, filter)
import Data.Maybe (maybe)
import Pux.Html (Html, td, text, tr, tbody, th, thead, table, h1, div)
import Pux.Router (link)

import Api.Models (Category(..), Product(..))
import Categories.Models (CategoryData(..))
import Categories.Messages (Msg)
import Router (Route(CategoryDetail), reverse)

view :: CategoryData -> Html Msg
view (CategoryData model) =
    div []
        [ h1 [] [ text "Categories" ]
        , catTable model.categories model.products
        ]


catTable :: List Category -> List Product -> Html Msg
catTable categories products =
    table []
        [ thead []
            [ tr []
                [ th [] [ text "Name" ]
                , th [] [ text "Categories" ]
                , th [] [ text "Products" ]
                ]
            ]
        , tbody [] <<< fromFoldable $ map (catRow categories products) categories
        ]


catRow :: List Category -> List Product -> Category -> Html Msg
catRow categories products category@(Category {id, name}) =
    tr []
        [ td [] [ link (reverse $ CategoryDetail id) [] [ text name ] ]
        , td [] [ text <<< show $ childCount category categories ]
        , td [] [ text <<< show $ productCount category products ]
        ]


childCount :: Category -> List Category -> Int
childCount (Category category) =
    length <<< filter (\(Category cat) -> maybe false ((==) category.id) cat.parent)


productCount :: Category -> List Product -> Int
productCount (Category category) =
    length <<< filter (\(Product prod) -> prod.category == category.id)
