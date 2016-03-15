{-# LANGUAGE ScopedTypeVariables #-}
module Handlers.Categories (categoryHandlers) where

import Import


categoryHandlers :: OM a
categoryHandlers = categoryListHandlers >> categoryDetailHandlers

categoryListHandlers :: OM a
categoryListHandlers = do
    get root $ listAndWrap [Asc CategoryName]
    post root $ do
        JSONObject (newCategory :: Category) <- jsonBody'
        category <- runSQL $ insertEntity newCategory
        json $ JSONList [category]

categoryDetailHandlers :: OM a
categoryDetailHandlers = do
    get var $ \catId -> getAndWrap (toSqlKey catId :: Key Category)
    put var $ \catId   -> updateAndWrap (toSqlKey catId :: Key Category)
    delete var $ \catId -> deleteAndReturn (toSqlKey catId :: Key Category)
