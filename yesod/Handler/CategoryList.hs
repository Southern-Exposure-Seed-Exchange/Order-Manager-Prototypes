module Handler.CategoryList where

import Import

getCategoryListR :: Handler Value
getCategoryListR = do
        cats <- runDB (selectList [] [Desc CategoryName])
        returnJson $ object ["category" .= cats]

postCategoryListR :: Handler Value
postCategoryListR = do
        category <- requireJsonBody :: Handler Category
        insertedCategory <- runDB $ insertEntity category
        returnJson insertedCategory
