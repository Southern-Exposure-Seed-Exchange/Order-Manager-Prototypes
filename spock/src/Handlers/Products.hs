{-# LANGUAGE ScopedTypeVariables #-}
module Handlers.Products (productHandlers) where

import Import
--import qualified Database.Persist as P        (delete)


productHandlers :: OM a
productHandlers = productListHandlers

productListHandlers :: OM a
productListHandlers = do
    get root $ listAndWrap [Asc ProductName]
    post root $ do
        [newProduct] :: [Product] <- jsonBody'
        insertedProduct <- runSQL $ insertEntity newProduct
        json $ JSONList [insertedProduct]
