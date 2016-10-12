module Categories.Commands where

import Prelude
import Control.Monad.Aff (Aff, attempt)
import Data.Argonaut.Decode (decodeJson, (.?))
import Data.Either (Either(..), either)
import Data.List (List)
import Network.HTTP.Affjax (AJAX)

import Api.Http (get)
import Api.Models (Category)
import Categories.Messages (Msg(..))


fetchCategories :: forall e. Aff ( ajax :: AJAX | e ) Msg
fetchCategories =
    do response <- attempt $ get "/api/categories"
       let decode res =
            do obj <- decodeJson res.response
               list <- obj .? "category"
               decodeJson list :: Either String (List Category)
           categories = either (Left <<< show) decode response
       pure $ ReceiveCategories categories
