module Categories.Commands where

import Prelude
import Control.Monad.Aff (Aff, attempt)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, (.?))
import Data.Either (Either(..), either)
import Network.HTTP.Affjax (AJAX, AffjaxResponse)

import Api.Http (get)
import Categories.Messages (Msg(..))
import Categories.Models (CategoryData(..))


fetchCategories :: forall e. Aff ( ajax :: AJAX | e ) Msg
fetchCategories =
    do response <- attempt $ get "/api/categories"
       let categoryData = either (Left <<< show) decodeCategoryData response
       pure $ ReceiveCategories categoryData


fetchCategory :: forall e. Int -> Aff ( ajax :: AJAX | e ) Msg
fetchCategory id =
    do response <- attempt $ get $ "/api/categories/" <> show id
       let categoryData = either (Left <<< show) decodeCategoryData response
       pure $ ReceiveCategory categoryData


decodeCategoryData :: AffjaxResponse Json -> Either String CategoryData
decodeCategoryData res = do
    obj <- decodeJson res.response
    catList <- obj .? "category"
    prodList <- obj .? "product"
    categories <- decodeJson catList
    products <- decodeJson prodList
    pure $ CategoryData { categories, products }
