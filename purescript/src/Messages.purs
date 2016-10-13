module Messages where

import Categories.Messages as CatMsg
import Router (Route)


data Msg
    = PageView Route
    | CategoriesMsg CatMsg.Msg
