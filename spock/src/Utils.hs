{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeFamilies     #-}
module Utils where
import Data.Aeson                                   (ToJSON(..), Value(..), FromJSON)
import Data.Aeson.Types                             (emptyObject)
import Data.HashMap.Strict                          (union)
import Database.Persist.Sqlite               hiding (get, delete)
import Network.HTTP.Types                           (notFound404)
import Web.Spock.Safe
import qualified Database.Persist.Sqlite         as Sql (get, delete)

import Models.Base
import Types


-- | Get a single Entity, sideload any related Entities, & wrap them in
-- a JSON object.
getAndWrap :: (Sideload r, ToJSON (Entity r), Named (Entity r), PersistEntity r,
               PersistEntityBackend r ~ SqlBackend)
           => Key r -> OMRoute ctx m b
getAndWrap key = getOr404 key $ \value -> do
        sideloadedData <- sideloads [key]
        json $ mergeObjects (toJSON $ JSONList [Entity key value]) sideloadedData


-- | Get all Entities, sideload any related Entities, & wrap them in a
-- JSON object.
listAndWrap :: (Sideload r, Named (Entity r), ToJSON (Entity r), PersistEntity r,
                PersistEntityBackend r ~ SqlBackend)
            => [SelectOpt r] -> OMRoute ctx m b
listAndWrap ordering = do
        items <- runSQL $ selectList [] ordering
        let itemKeys = map (\(Entity k _) -> k) items
        sideloadedData <- sideloads itemKeys
        json $ mergeObjects (toJSON $ JSONList items) sideloadedData


-- | Update an Entity using a Key & JSON request, wrapping it in an object.
updateAndWrap :: (ToJSON (Entity r), Named r, Sideload r, FromJSON r,
                  PersistEntity r, PersistEntityBackend r ~ SqlBackend)
              => Key r -> OMRoute ctx m b
updateAndWrap key = getOr404 key $ \_ -> do
        JSONObject newItem <- jsonBody'
        runSQL $ replace key newItem
        sideloadedData <- sideloads [key]
        json $ mergeObjects (toJSON $ JSONList [Entity key newItem]) sideloadedData


-- | Delete an Entity and return an empty JSON object.
deleteAndReturn :: (PersistEntityBackend r ~ SqlBackend, PersistEntity r)
                =>  Key r -> OMRoute ctx m b
deleteAndReturn key = runSQL (Sql.delete key) >> json emptyObject



-- | Get an Entity and perform an action with it, return a 404 if it does
-- not exist.
getOr404 :: (PersistEntity r, PersistEntityBackend r ~ SqlBackend)
         => Key r -> (r -> OMRoute ctx m b) -> OMRoute ctx m b
getOr404 key action = do
        maybeValue <- runSQL $ Sql.get key
        case maybeValue of
            Nothing -> setStatus notFound404 >> text "not found"
            Just v  -> action v


-- | Merge the keys of two objects, preferring the keys in the first
-- argument. If Objects are not passed, the first argument is returned.
mergeObjects :: (ToJSON a, ToJSON b) => a -> b -> Value
mergeObjects a b = case (toJSON a, toJSON b) of
        (Object o1, Object o2) -> Object $ union o1 o2
        (x,_)                  -> x
