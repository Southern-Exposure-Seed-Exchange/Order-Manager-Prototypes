module Categories.List where

import Prelude hiding (div)
import Data.Array (fromFoldable)
import Data.List (List, length, filter)
import Data.Maybe (maybe)
import Pux.Html (Html, td, text, tr, tbody, th, thead, table, h1, div, button)
import Pux.Html.Events (onClick)

import Api.Models (Category(..), Product(..))
import Categories.Messages (Msg(..))
import Model (Model)

view :: Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Categories" ]
        , catTable model
        , button [onClick (const FetchCategories)] [ text "Fetch" ]
        ]


catTable :: Model -> Html Msg
catTable model =
    table []
        [ thead []
            [ tr []
                [ th [] [ text "Name" ]
                , th [] [ text "Categories" ]
                , th [] [ text "Products" ]
                ]
            ]
        , tbody [] <<< fromFoldable $ map (catRow model) model.categories
        ]


catRow :: Model -> Category -> Html Msg
catRow model category@(Category {name}) =
    tr []
        [ td [] [ text name ]
        , td [] [ text <<< show $ childCount category model.categories ]
        , td [] [ text <<< show $ productCount category model.products ]
        ]


childCount :: Category -> List Category -> Int
childCount (Category category) =
    length <<< filter (\(Category cat) -> maybe false ((==) category.id) cat.parent)


productCount :: Category -> List Product -> Int
productCount (Category category) =
    length <<< filter (\(Product prod) -> prod.category == category.id)
