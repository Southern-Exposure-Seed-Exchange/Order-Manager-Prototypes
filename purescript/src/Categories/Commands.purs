module Categories.Commands where

import Prelude
import Control.Monad.Aff (Aff, attempt)
import Data.Argonaut.Decode (decodeJson, (.?))
import Data.Either (Either(..), either)
import Network.HTTP.Affjax (AJAX)

import Api.Http (get)
import Categories.Messages (Msg(..))
import Categories.Models (CategoryData(..))


fetchCategories :: forall e. Aff ( ajax :: AJAX | e ) Msg
fetchCategories =
    do response <- attempt $ get "/api/categories"
       let decode res =
            do obj <- decodeJson res.response
               catList <- obj .? "category"
               prodList <- obj .? "product"
               categories <- decodeJson catList
               products <- decodeJson prodList
               pure $ CategoryData { categories, products }
           categoryData = either (Left <<< show) decode response :: Either String CategoryData
       pure $ ReceiveCategories categoryData
