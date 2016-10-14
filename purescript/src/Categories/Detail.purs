module Categories.Detail where

import Prelude ((==), ($), (<<<), (<>), const)
import Data.List (head, filter, null)
import Data.Maybe (Maybe(..), maybe)
import Pux.Html ( Html, div, h1, h2, text, button, small, p)
import Pux.Html.Events (onClick)
import Pux.Router (link)

import Api.Models (Category(..), Product(..))
import Categories.List (catTable)
import Categories.Messages (Msg(DeleteCategory))
import Categories.Models (CategoryData(..))
import Products.List (productsTable)
import Router (Route(CategoryDetail), reverse)


view :: Category -> CategoryData -> Html Msg
view (Category category) (CategoryData model) =
    div []
        [ h1 []
            [ text $ category.name
            , parentCategory
            ]
        , button [] [ text "Edit" ]
        , button [ onClick <<< const $ DeleteCategory category.id] [ text "Delete" ]
        , subCategoryTable
        , subProductTable
        ]
    where
        parentCategory =
            let maybeParent =
                    head $ filter (\(Category c) -> maybe false ((==) c.id) category.parent) model.categories
             in case maybeParent of
                Nothing ->
                    text ""
                Just (Category parent) ->
                    small []
                        [ text " "
                        , link (reverse $ CategoryDetail parent.id) []
                            [ text $ "(" <> parent.name  <> ")"]
                        ]
        subCategoryTable =
            if null subCategories
            then
                text ""
            else
                div []
                    [ h2 [] [ text "Categories" ]
                    , catTable subCategories model.products
                    ]
        subCategories =
            filter (\(Category c) -> maybe false ((==) category.id) c.parent)
                   model.categories
        subProductTable =
            if null subProducts
            then
                p [] [ text "This Category has no Products." ]
            else
                div []
                    [ h2 [] [ text "Products" ]
                    , productsTable subProducts
                    ]
        subProducts =
            filter (\(Product p) -> p.category == category.id) model.products
