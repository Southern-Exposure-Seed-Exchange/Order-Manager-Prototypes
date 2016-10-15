module View where

import Prelude (map, ($), (==), (<<<))
import Data.List (head, filter)
import Data.Maybe (Maybe(..))
import Pux.Html (Html, div, h1, text, ul, li)
import Pux.Router (link)

import Api.Models (Category(..))
import Categories.Detail as CatDetail
import Categories.List as CatList
import Classes (fromModel)
import Messages (Msg(..))
import Model (Model)
import Products.List as ProdList
import Router (Route(..), reverse)


view :: Model -> Html Msg
view model =
    div []
        [ nav
        , page model
        ]


nav :: Html Msg
nav =
    ul []
        [ li [] [ link (reverse Home) [] [text "Home" ] ]
        , li [] [ link (reverse Categories) [] [text "Categories" ] ]
        , li [] [ link (reverse Products) [] [text "Products" ] ]
        ]


page :: Model -> Html Msg
page model =
    case model.route of
        Home ->
            div [] [ h1 [] [ text "Home" ] ]
        Categories ->
            map CategoriesMsg <<< CatList.view $ fromModel model
        CategoryDetail id ->
            let maybeCategory =
                    head $ filter (\(Category c) -> c.id == id) model.categories
             in case maybeCategory of
                    Nothing ->
                        notFoundView
                    Just category ->
                        map CategoriesMsg <<< CatDetail.view category $
                        fromModel model
        Products ->
            map ProductsMsg <<< ProdList.view $ fromModel model
        NotFound ->
            notFoundView


notFoundView :: forall msg. Html msg
notFoundView =
    div [] [ h1 [] [ text "404 - Not Found" ] ]
