module Categories.Messages where

import Data.Either (Either)

import Categories.Models (CategoryData)


data Msg
    = FetchCategories
    | ReceiveCategories (Either String CategoryData)
