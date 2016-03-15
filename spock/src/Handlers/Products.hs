{-# LANGUAGE ScopedTypeVariables #-}
module Handlers.Products (productHandlers) where

import Import


productHandlers :: OM a
productHandlers = productListHandlers >> productDetailHandlers

productListHandlers :: OM a
productListHandlers = do
    get root $ listAndWrap [Asc ProductName]
    post root $ do
        JSONObject (newProduct :: Product) <- jsonBody'
        insertedProduct <- runSQL $ insertEntity newProduct
        json $ JSONList [insertedProduct]

productDetailHandlers :: OM a
productDetailHandlers = do
    get var $ \prodId -> getAndWrap (toSqlKey prodId :: Key Product)
    put var $ \prodId -> updateAndWrap (toSqlKey prodId :: Key Product)
    delete var $ \prodId -> deleteAndReturn (toSqlKey prodId :: Key Product)
