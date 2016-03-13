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
        where filterParams    = ["parent"]
              filterHandlers  = [parentFilter]
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
        CategoryList [category] <- requireJsonBody
        isValid <- validate category
        case isValid of
            Right c -> do
                newCategory <- runDB (insertEntity c)
                returnJson $ object ["category" .= newCategory]
            Left errors -> sendResponseStatus (Status 422 "invalid") $ object [ "errors" .= [errors] ]

newtype CategoryList = CategoryList [Category]
instance FromJSON CategoryList where
        parseJSON (Object o) = do
            categoryJson <- o .: "category"
            c <- parseJSON categoryJson
            return $ CategoryList [c]
        parseJSON _ = mzero
