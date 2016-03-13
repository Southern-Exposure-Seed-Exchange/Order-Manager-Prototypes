{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Utils where
import qualified Data.Text                     as T
import Data.Aeson                                   (ToJSON(..), (.=), object)
import Database.Persist.Sqlite hiding               (get, delete)
import Network.HTTP.Types   (notFound404)
import Web.Spock.Safe
import qualified Database.Persist.Sqlite     as Sql (get)

import Types


getOr404 :: (PersistEntity r, PersistEntityBackend r ~ SqlBackend) =>
            Key r -> (r -> OMRoute ctx m b) -> OMRoute ctx m b
getOr404 key action = do
        maybeValue <- runSQL $ Sql.get key
        case maybeValue of
            Nothing -> setStatus notFound404 >> text "not found"
            Just v  -> action v

getAndWrap :: (ToJSON (Entity r), PersistEntity r, PersistEntityBackend r ~ SqlBackend) =>
              T.Text -> Key r -> OMRoute ctx m b
getAndWrap name key = getOr404 key $
        \value -> json $ object [name .= toJSON (Entity key value)]
