module Categories.Commands where

import Prelude
import Control.Monad.Aff (Aff, attempt)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, (.?))
import Data.Either (Either(..), either)
import Network.HTTP.Affjax (AJAX, AffjaxResponse, Affjax)

import Api.Http (delete, get, Endpoint(..))
import Categories.Messages (Msg(..))
import Categories.Models (CategoryData(..))


fetchCategories :: forall e. Aff ( ajax :: AJAX | e ) Msg
fetchCategories =
    do response <- attempt $ get CategoriesEndpoint
       let categoryData = either (Left <<< show) decodeCategoryData response
       pure $ ReceiveCategories categoryData


fetchCategory :: forall e. Int -> Aff ( ajax :: AJAX | e ) Msg
fetchCategory id =
    do response <- attempt <<< get $ CategoryEndpoint id
       let categoryData = either (Left <<< show) decodeCategoryData response
       pure $ ReceiveCategory categoryData


deleteCategory :: forall e. Int -> Aff ( ajax :: AJAX | e ) Msg
deleteCategory id =
    do response <- attempt $ delete (CategoryEndpoint id) :: forall e1. Affjax e1 Json
       let responseData = either (Left <<< show) (const $ Right id) response
       pure $ DeletedCategory (Right id)


decodeCategoryData :: AffjaxResponse Json -> Either String CategoryData
decodeCategoryData res = do
    obj <- decodeJson res.response
    catList <- obj .? "category"
    prodList <- obj .? "product"
    categories <- decodeJson catList
    products <- decodeJson prodList
    pure $ CategoryData { categories, products }
