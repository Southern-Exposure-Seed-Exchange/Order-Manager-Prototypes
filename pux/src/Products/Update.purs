module Products.Update where

import Prelude (($))
import Data.Either (Either(..))
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects)

import Api.Models (Product(..), ProductVariant(..), Category(..))
import Products.Messages (Msg(..))
import Products.Models (ProductData(..))
import Utils (replaceAll, sortByField)

update :: forall e. Msg -> ProductData -> EffModel ProductData  Msg ( ajax :: AJAX | e )
update (ReceiveProducts (Left _)) model =
    noEffects model
update (ReceiveProducts (Right newData)) model =
    noEffects $ updateProductData model newData


updateProductData :: ProductData -> ProductData -> ProductData
updateProductData (ProductData oldData) (ProductData newData) =
    ProductData $ oldData
        { products = sortByField (\(Product p) -> p.name) $
                     replaceAll oldData.products newData.products
        , variants = sortByField (\(ProductVariant p) -> p.sku) $
                     replaceAll oldData.variants newData.variants
        , categories = sortByField (\(Category c) -> c.name) $
                       replaceAll oldData.categories newData.categories
        }
