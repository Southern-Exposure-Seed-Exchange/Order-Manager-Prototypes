{-# LANGUAGE ScopedTypeVariables #-}
module Handler.CategoryList where

import Import
import Database.Persist.Sql (toSqlKey)
import Prelude (read)

getCategoryListR :: Handler Value
getCategoryListR = do
        filters <- createFilterList $ zip filterParams filterHandlers
        cats <- runDB $ selectList filters [Asc CategoryName]
        (prods :: [Entity Product]) <- runDB $ selectList [] []
        returnJson $ object ["category" .= cats, "products" .= prods]
        where filterParams = ["parent"]
              filterHandlers = [parentFilter]
              parentFilter "" = [CategoryParent ==. Nothing]
              parentFilter s  =
                case read (unpack s) of
                    Nothing -> []
                    Just i  -> [CategoryParent ==. Just (toSqlKey i)]

createFilterList :: [(Text, Text -> [Filter val])] -> Handler [Filter val]
createFilterList = foldM applyFilter []
        where filterParam param = concat ["filter[", param, "]"]
              applyFilter acc (name, handler) = do
                    value <- lookupGetParam $ filterParam name
                    case value of
                        Nothing -> return acc
                        Just x  -> return (handler x <> acc)


postCategoryListR :: Handler Value
postCategoryListR = do
        category <- requireJsonBody :: Handler Category
        insertedCategory <- runDB $ insertEntity category
        returnJson insertedCategory
