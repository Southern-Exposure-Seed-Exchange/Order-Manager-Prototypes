{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Models where

import Control.Monad                (mzero)
import Control.Monad.Reader         (ReaderT)
import Data.Aeson                   ( FromJSON(..), (.:), Value(..), ToJSON(..)
                                    , (.=), object)
import Data.Int                     (Int64)
import Data.Proxy                   (Proxy(..))
import Database.Persist
import Database.Persist.Postgresql  (SqlBackend(..), runMigration)
import Database.Persist.TH          ( share, mkPersist, sqlSettings, mkMigrate
                                    , persistLowerCase)
import qualified Data.Text       as T


type Milligrams = Int64
type Cents = Int64

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Category json
    name T.Text
    description T.Text
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
    isActive Bool default=true
    UniqueProduct name
    deriving Show

ProductVariant json
    product ProductId
    sku T.Text
    weight Milligrams
    price Cents
    UniqueVariant sku
    deriving Show
|]

doMigrations :: ReaderT SqlBackend IO ()
doMigrations = runMigration migrateAll


-- | The Named typeclass is used to automatically generate the name of the
-- key to nest the object(s) under when returning it in a JSON response.
-- For example, a Category will be nested under the "category" key of the
-- response object.
class Named a where
        name :: Proxy a -> T.Text

instance Named Category where name _ = "category"
instance Named Product where name _ = "product"
instance Named ProductVariant where name _ = "productVariant"
-- | The `Entity a` instance of the Named typeclass automatically applies
-- the base Model's Named instance instead of requiring us to define
-- a Named instance for both a `Category` and an `Entity Category`.
instance Named a => Named (Entity a) where
        name _ = name (Proxy :: Proxy a)

data JSONList a = JSONList [a]
instance (FromJSON a, Named a) => FromJSON (JSONList a) where
        parseJSON (Object o) = do
            named <- o .: name (Proxy :: Proxy a) >>= parseJSON
            return $ JSONList [named]
        parseJSON _          = mzero
instance (ToJSON a, Named a) => ToJSON (JSONList a) where
        toJSON (JSONList l)  = object
            [name (Proxy :: Proxy a) .= map toJSON l]

data JSONObject a = JSONObject a
instance (FromJSON a, Named a) => FromJSON (JSONObject a) where
        parseJSON (Object o) = do
            named <- o .: name (Proxy :: Proxy a) >>= parseJSON
            return $ JSONObject named
        parseJSON _          = mzero
instance (ToJSON a, Named a) => ToJSON (JSONObject a) where
        toJSON (JSONObject a)  = object
            [name (Proxy :: Proxy a) .= toJSON a]
