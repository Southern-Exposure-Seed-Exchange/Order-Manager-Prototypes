module Products.Messages where

import Data.Either (Either)

import Products.Models (ProductData)


data Msg
    = ReceiveProducts (Either String ProductData)
