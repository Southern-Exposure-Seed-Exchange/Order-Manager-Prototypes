{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Types where
import Control.Monad.IO.Class           (MonadIO)
import Control.Monad.Logger             (runNoLoggingT, NoLoggingT)
import Control.Monad.Trans.Resource     (runResourceT, ResourceT)
import Database.Persist.Sqlite          (runSqlConn, SqlBackend, SqlPersistT)
import Web.Spock.Safe                   (SpockCtxT)
import Web.Spock.Shared                 ( runQuery, SpockConn, HasSpock
                                        , ActionCtxT, WebStateM)

type OM a = SpockCtxT () (WebStateM SqlBackend (Maybe a) OMState) ()

type OMRoute ctx m b = (MonadIO m) => OMSqlResult (ActionCtxT ctx m) b
data OMState = OMState { }

type OMSQL a = SqlPersistT (NoLoggingT (ResourceT IO)) a
type OMSqlResult m a = (HasSpock m, SpockConn m ~ SqlBackend) => m a


runSQL :: OMSQL a -> OMSqlResult m a
runSQL action = runQuery $ \conn -> runResourceT $ runNoLoggingT $ runSqlConn action conn
