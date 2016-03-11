module Handler.ProductVariantList where

import Import

getProductVariantListR :: Handler Value
getProductVariantListR = do
        prods <- runDB $ selectList [] [Asc ProductVariantProduct, Asc ProductVariantSku]
        returnJson $ object ["productVariant" .= prods]

postProductVariantListR :: Handler Html
postProductVariantListR = error "Not yet implemented: postProductVariantListR"
