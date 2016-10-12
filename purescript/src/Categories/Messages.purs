module Categories.Messages where

import Data.Either (Either)
import Data.List (List)

import Api.Models (Category)


data Msg
    = FetchCategories
    | ReceiveCategories (Either String (List Category))
