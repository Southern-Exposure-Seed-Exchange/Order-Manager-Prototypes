module Messages where

import Categories.Messages as CatMsg
import Products.Messages as ProdMsg
import Router (Route)


data Msg
    = PageView Route
    | CategoriesMsg CatMsg.Msg
    | ProductsMsg ProdMsg.Msg
