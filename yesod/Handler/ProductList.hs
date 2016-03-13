module Handler.ProductList where

import Import

getProductListR :: Handler Value
getProductListR = do
        prods <- runDB $ selectList [] [Asc ProductName]
        variants <- runDB $ selectList [] [Asc ProductVariantSku]
        returnJson $ object ["product" .= prods, "productVariant" .= variants]

postProductListR :: Handler Value
postProductListR = do
        ProductList [p] <- requireJsonBody
        insertedProduct <- runDB $ insertEntity p
        returnJson insertedProduct

newtype ProductList = ProductList [Product]
instance FromJSON ProductList where
        parseJSON (Object o) = do
            productJson <- o .: "product"
            p <- parseJSON productJson
            return $ ProductList [p]
        parseJSON _ = mzero
