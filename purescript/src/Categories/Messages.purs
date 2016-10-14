module Categories.Messages where

import Prelude (Unit)
import Data.Either (Either)

import Api.Models (CategoryId)
import Categories.Models (CategoryData)


data Msg
    = DeleteCategory CategoryId
    | ReceiveCategories (Either String CategoryData)
    | ReceiveCategory (Either String CategoryData)
    | DeletedCategory (Either String CategoryId)
    | NoOp Unit
