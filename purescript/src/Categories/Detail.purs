module Categories.Detail where

import Prelude ((==), ($), show, map, (<<<), (<>), const)
import Data.Array (fromFoldable)
import Data.List (List, head, filter, null)
import Data.Maybe (Maybe(..), maybe)
import Pux.Html ( Html, div, h1, h2, text, button, table, tbody, thead, tr
                , td, th, small, p)
import Pux.Html.Events (onClick)
import Pux.Router (link)

import Api.Models (Category(..), Product(..))
import Categories.List (catTable)
import Categories.Messages (Msg(DeleteCategory))
import Model (Model)
import Router (Route(CategoryDetail), reverse)


view :: Category -> Model -> Html Msg
view (Category category) model =
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

productsTable :: List Product -> Html Msg
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


prodRow :: Product -> Html Msg
prodRow (Product product) =
    tr []
        [ td [] [ text product.name ]
        , td [ ] [ text $ show product.isOrganic ]
        , td [ ] [ text $ show product.isHeirloom ]
        , td [ ] [ text $ show product.isSouthEast ]
        , td [ ] [ text $ show product.isActive ]
        ]
