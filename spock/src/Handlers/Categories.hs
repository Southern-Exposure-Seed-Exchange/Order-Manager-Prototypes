{-# LANGUAGE OverloadedStrings #-}
module Handlers.Categories (categoryHandlers) where

import Import
import qualified Database.Persist as P        (delete)


categoryHandlers :: OM a
categoryHandlers = categoryListHandlers >> categoryDetailHandlers

categoryListHandlers :: OM a
categoryListHandlers = do
    get root $ do
        categories <- runSQL $ selectList [] [Asc CategoryName]
        json $ object ["categories" .= toJSON categories]
    post root $ do
        CategoryList [newCategory] <- jsonBody'
        category <- runSQL $ insertEntity newCategory
        json $ object ["categories" .= category]

categoryDetailHandlers :: OM a
categoryDetailHandlers = do
    get var $ \catId ->
        getAndWrap "categories" (toSqlKey catId :: Key Category)
    put var $ \catId ->
        let key = toSqlKey catId :: Key Category in
        getOr404 key $ \_ -> do
            CategoryList [newCategory] <- jsonBody'
            runSQL $ replace key newCategory
            json $ object ["categories" .= Entity key newCategory]
    delete var $ \catId -> do
        runSQL $ P.delete (toSqlKey catId :: Key Category)
        json $ object []
