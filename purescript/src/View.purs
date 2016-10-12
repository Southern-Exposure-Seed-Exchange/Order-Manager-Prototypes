module View where

import Prelude
import Pux.Html (Html)

import Categories.List as CatList
import Messages (Msg(..))
import Model (Model)


view :: Model -> Html Msg
view model =
    map CategoriesMsg $ CatList.view model
