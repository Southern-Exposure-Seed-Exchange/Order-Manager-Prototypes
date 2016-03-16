{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where

import Control.Monad.IO.Class       (MonadIO)
import Control.Monad.Reader         (ReaderT, MonadReader, asks, liftIO)
import Control.Monad.Trans.Either   (EitherT)
import Data.Aeson                   (FromJSON(..), ToJSON(..))
import Data.Int                     (Int64)
import Database.Persist.Postgresql  (SqlPersistT, runSqlPool, ToBackendKey, Key, toSqlKey, SqlBackend)
import Servant

import Config       (Config(..))


type AppM = ReaderT Config (EitherT ServantErr IO)
type OMSQL m b = (MonadIO m, MonadReader Config m) => SqlPersistT IO b -> m b

runDB :: OMSQL m a
runDB query = asks getPool >>= liftIO . runSqlPool query


-- | A Persistent Key wrapped in a newtype so that we can derive the
-- FromText & ToText instances necessary for Servant's routing.
newtype PKey a = PKey { fromPKey :: Int64
                      } deriving (FromJSON, ToJSON, ToText, FromText)
-- | Retrieve a Persistent SQL Key out of a PKey value.
_PKey :: ToBackendKey SqlBackend a => PKey a -> Key a
_PKey pKey = toSqlKey $ fromPKey pKey
