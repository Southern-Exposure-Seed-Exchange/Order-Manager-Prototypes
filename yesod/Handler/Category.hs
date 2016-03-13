module Handler.Category where

import Import
import qualified Data.Text as Text

getCategoryR :: CategoryId -> Handler Value
getCategoryR categoryId = do
        category <- runDB $ get404 categoryId
        let categoryJSON = toJSON $ Entity categoryId category
        returnJson $ object ["category" .= categoryJSON]

postCategoryR :: CategoryId -> Handler Value
postCategoryR categoryId = do
        category <- requireJsonBody :: Handler Category
        updated <- runDB $ replace categoryId category >> get categoryId
        returnJson updated

deleteCategoryR :: CategoryId -> Handler Value
deleteCategoryR categoryId = do
        runDB $ delete categoryId
        returnJson $ object []
