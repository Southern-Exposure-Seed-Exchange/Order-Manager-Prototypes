module View where

import Prelude (map, ($))
import Pux.Html (Html, div, h1, text, ul, li)
import Pux.Router (link)

import Categories.List as CatList
import Messages (Msg(..))
import Router (Route(..), reverse)
import Model (Model)


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
        ]


page :: Model -> Html Msg
page model =
    case model.route of
        Home ->
            div [] [ h1 [] [ text "Home" ] ]
        Categories ->
            map CategoriesMsg $ CatList.view model
        NotFound ->
            div [] [ h1 [] [ text "404 - Not Found" ] ]
