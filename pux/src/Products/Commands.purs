module Products.Commands where

import Prelude (($), (<<<), (>>=), show, bind, pure)
import Control.Monad.Aff (Aff, attempt)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, (.?))
import Data.Either (Either(Left), either)
import Network.HTTP.Affjax (AJAX, AffjaxResponse)

import Api.Http (get, Endpoint(..))
import Products.Messages (Msg(..))
import Products.Models (ProductData(..))


fetchProducts :: forall e. Aff ( ajax :: AJAX | e ) Msg
fetchProducts =
    do response <- attempt $ get ProductsEndpoint
       let productData = either (Left <<< show) decodeProductData response
       pure $ ReceiveProducts productData


decodeProductData :: AffjaxResponse Json -> Either String ProductData
decodeProductData res = do
    obj <- decodeJson res.response
    products <- obj .? "product" >>= decodeJson
    variants <- obj .? "productVariant" >>= decodeJson
    categories <- obj .? "category" >>= decodeJson
    pure $ ProductData { products, variants, categories }
