{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Models.Base where

import Control.Monad                (mzero, (>=>))
import qualified Data.Text     as T

import Data.Aeson (FromJSON(..), (.:), Value(..), ToJSON(..), (.=), object)
import Data.Aeson.Types (emptyObject)
import Database.Persist.TH
import Database.Persist

import Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Category json
    name T.Text
    description T.Text default=''
    parent CategoryId Maybe
    UniqueCategory name
    deriving Show

Product json
    name T.Text
    description T.Text
    category CategoryId
    isOrganic Bool
    isHeirloom Bool
    isSouthEast Bool
    isActive Bool default=1
    UniqueProduct name
    deriving Show

ProductVariant json
    product ProductId
    sku T.Text
    weight Rational
    price Rational
    UniqueVariant sku
    deriving Show
|]


class Named a where
        usingName  :: (T.Text -> (a, b)) -> (a, b)
        usingNameM :: (T.Text -> m a) -> m a

instance Named Category where
        usingName = ($ "category")
        usingNameM = ($ "category")
instance Named (Entity Category) where
        usingName = ($ "category")
        usingNameM = ($ "category")
instance Named Product where
        usingName = ($ "product")
        usingNameM = ($ "product")
instance Named (Entity Product) where
        usingName = ($ "product")
        usingNameM = ($ "product")
instance Named ProductVariant where
        usingName = ($ "productVariant")
        usingNameM = ($ "productVariant")
instance Named (Entity ProductVariant) where
        usingName = ($ "productVariant")
        usingNameM = ($ "productVariant")

data JSONList a = JSONList [a]
instance (FromJSON a, Named a) => FromJSON (JSONList a) where
        parseJSON (Object o) = do
            named <- usingNameM $ (o .:) >=> parseJSON
            return $ JSONList [named]
        parseJSON _          = mzero

instance (ToJSON a, Named a) => ToJSON (JSONList a) where
        toJSON (JSONList []) = emptyObject
        toJSON (JSONList l@(x:_)) = object
            [snd (usingName $ \n -> (x, n)) .= map toJSON l]



class Sideload a  where
        sideloads :: [Key a] -> OMRoute ctx m Value

instance Sideload Product where
        sideloads productIds = do
            variants <- runSQL $ selectList [ProductVariantProduct <-. productIds]
                                            [Asc ProductVariantSku]
            return . toJSON $ JSONList variants
instance Sideload Category where
        sideloads categoryIds = do
            products <- runSQL $ selectList [ProductCategory <-. categoryIds]
                                            [Asc ProductName]
            return . toJSON $ JSONList products
