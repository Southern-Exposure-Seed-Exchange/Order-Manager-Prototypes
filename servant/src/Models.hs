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
import Data.List                    (nub)
import Data.Maybe                   (mapMaybe)
import Database.Persist
import Database.Persist.Postgresql  (SqlBackend(..), runMigration)
import Database.Persist.TH          ( share, mkPersist, sqlSettings, mkMigrate
                                    , persistLowerCase)
import Database.Persist.Types       (SelectOpt(..))
import qualified Data.Text       as T
import qualified Data.HashMap.Strict    as HM

import Types


type Milligrams = Int64
type Cents = Int64

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Category json
    name T.Text
    description T.Text
    parent CategoryId Maybe
    UniqueCategory name
    deriving Show Eq

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
    quantity Int64
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


-- | The `Sideloaded a` type represents a list of Entities that have had
-- their related data loaded. The sideloaded `Value` should be a JSON
-- object that will be merged into the final JSONList.
data Sideloaded a = Sideloaded (JSONList a, Value)
-- | The `Sideloaded a` instance of the ToJSON typeclass merges the
-- sideloaded data with the Entity into a single object.
instance (ToJSON a, Named a) => ToJSON (Sideloaded a) where
        toJSON (Sideloaded (ent, side)) = mergeObjects ent side

-- | The Sideload typeclass is used by the `listRoute` to pull the
-- sideloaded data for an Entity type from the database.
class (ToJSON a, Named a) => Sideload a where
        -- | The default action is to pull nothing from the database.
        sideload :: [a] -> AppM (Sideloaded a)
        sideload x = return $ Sideloaded (JSONList x, object [])

-- | Categories sideload their Products.
instance Sideload (Entity Category) where
        sideload cs =
            do prods <- runDB $ selectList [ProductCategory <-. catIds] [defaultOrdering]
               parents <- runDB $ selectList [CategoryId <-. parentIds] []
               children <- runDB $ selectList [CategoryParent <-. map Just catIds] []
               return $ Sideloaded (JSONList (nub $ cs ++ parents ++ children),
                                    toJSON $ JSONList prods)
            where catIds = nub $ map (\(Entity i _) -> i) cs
                  parentIds = mapMaybe (\(Entity _ e) -> categoryParent e) cs
-- | Products sideload their Categories, & Variants.
instance Sideload (Entity Product) where
        sideload ps = do
            cats <- runDB $ selectList [CategoryId <-. catIds] [defaultOrdering]
            vars <- runDB $ selectList [ProductVariantProduct <-. prodIds] [defaultOrdering]
            return $ Sideloaded (JSONList ps, mergeObjects (JSONList cats) (JSONList vars))
            where catIds = nub $ map (\(Entity _ prod) -> productCategory prod) ps
                  prodIds = nub $ map (\(Entity i _) -> i) ps
-- | Product Variants sideload their Products.
instance Sideload (Entity ProductVariant) where
        sideload pvs = do
            prods <- runDB $ selectList [ProductId <-. prodIds] [defaultOrdering]
            return $ Sideloaded (JSONList pvs, toJSON $ JSONList prods)
            where prodIds = nub $ map (\(Entity _ pv) -> productVariantProduct pv) pvs


-- | Merge the keys of two JSON Objects, or simply return the first JSON
-- value if two Objects are not passed.
mergeObjects :: (ToJSON a, ToJSON b) => a -> b -> Value
mergeObjects obj1 obj2 = case (toJSON obj1, toJSON obj2) of
        (Object o1, Object o2) -> Object $ HM.union o1 o2
        (x, _)                 -> x


-- | The `DefaultOrdering a` type represents the default sorting field that
-- should be used when fetching data from the database.
class DefaultOrdering a where
        defaultOrdering :: SelectOpt a

-- | Categories are ordered by their Name.
instance DefaultOrdering Category where
        defaultOrdering = Asc CategoryName

-- | Products are ordered by their Name.
instance DefaultOrdering Product where
        defaultOrdering = Asc ProductName

-- | ProductVariants are ordered by their SKU
instance DefaultOrdering ProductVariant where
        defaultOrdering = Asc ProductVariantSku
