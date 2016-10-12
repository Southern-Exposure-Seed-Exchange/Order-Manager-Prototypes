module Messages where

import Categories.Messages as CatMsg


data Msg
    = CategoriesMsg CatMsg.Msg
