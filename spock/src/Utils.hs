{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Utils where
import Data.Aeson                                   ( ToJSON(..), (.=), object
                                                    , Value(..))
import Data.HashMap.Strict                          (union)
import Database.Persist.Sqlite               hiding (get, delete)
import Network.HTTP.Types                           (notFound404)
import Web.Spock.Safe
import qualified Data.Text                       as T
import qualified Database.Persist.Sqlite         as Sql (get)

import Types

import Models.Base


getOr404 :: (PersistEntity r, PersistEntityBackend r ~ SqlBackend)
         => Key r -> (r -> OMRoute ctx m b) -> OMRoute ctx m b
getOr404 key action = do
        maybeValue <- runSQL $ Sql.get key
        case maybeValue of
            Nothing -> setStatus notFound404 >> text "not found"
            Just v  -> action v

listAndWrap :: (Sideload r, Named (Entity r), ToJSON (Entity r), PersistEntity r,
                PersistEntityBackend r ~ SqlBackend)
            => [SelectOpt r] -> OMRoute ctx m b
listAndWrap ordering = do
        items <- runSQL $ selectList [] ordering
        let itemKeys = map (\(Entity k _) -> k) items
        sideloadedData <- sideloads itemKeys
        json $ mergeObjects (toJSON $ JSONList items) sideloadedData

getAndWrap :: (ToJSON (Entity r), PersistEntity r, PersistEntityBackend r ~ SqlBackend)
           => T.Text -> Key r -> OMRoute ctx m b
getAndWrap name key = getOr404 key $
        \value -> json $ object [name .= toJSON (Entity key value)]

-- | Merge the keys of two objects, preferring the keys in the first
-- argument. If Objects are not passed, the first argument is returned.
mergeObjects :: Value -> Value -> Value
mergeObjects (Object o1) (Object o2) = Object $ union o1 o2
mergeObjects x _ = x

