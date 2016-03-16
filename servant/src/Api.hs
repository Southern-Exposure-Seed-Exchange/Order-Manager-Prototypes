{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
module Api (app) where

import Control.Monad                (void)
import Control.Monad.IO.Class       (MonadIO)
import Control.Monad.Reader         (ReaderT, runReaderT, lift, MonadReader, asks, liftIO)
import Control.Monad.Trans.Either   (EitherT, left)
import Data.Int                     (Int64)
import Database.Persist.Postgresql
import Network.Wai                  (Application)
import Servant

import Config       (Config(..))
import Models

type AppM = ReaderT Config (EitherT ServantErr IO)

api :: Proxy API
api = Proxy

type OMSQL m b = (MonadIO m, MonadReader Config m) => SqlPersistT IO b -> m b
runDB :: OMSQL m a
runDB query = do
        pool <- asks getPool
        liftIO $ runSqlPool query pool

readerToEither :: Config -> AppM :~> EitherT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg

readerServer :: Config -> Server API
readerServer cfg = enter (readerToEither cfg) categoryRoutes

app :: Config -> Application
app cfg = serve api $ readerServer cfg


type API = "categories" :> CategoryAPI

type CategoryAPI =
        Get '[JSON] (JSONList (Entity Category))
   :<|> ReqBody '[JSON] (JSONObject Category)
        :> Post '[JSON] (JSONObject (Entity Category))
   :<|> Capture "id" Int64
        :> Get '[JSON] (JSONObject (Entity Category))
   :<|> Capture "id" Int64
        :> ReqBody '[JSON] (JSONObject Category)
        :> Put '[JSON] (JSONObject (Entity Category))
   :<|> Capture "id" Int64
        :> Delete '[JSON] ()

categoryRoutes = allCategories
            :<|> createCategory
            :<|> singleCategory
            :<|> updateCategory
            :<|> deleteCategory


allCategories :: AppM (JSONList (Entity Category))
allCategories = do
        categories <- runDB $ selectList [] []
        return $ JSONList categories

createCategory :: JSONObject Category -> AppM (JSONObject (Entity Category))
createCategory (JSONObject category) = do
        key <- runDB $ insert category
        return . JSONObject $ Entity key category

singleCategory :: Int64 -> AppM (JSONObject (Entity Category))
singleCategory categoryId = do
        let key = toSqlKey categoryId
        maybeCategory <- runDB $ get key
        case maybeCategory of
            Nothing -> lift $ left err404
            Just x -> return $ JSONObject (Entity key x)

updateCategory :: Int64 -> JSONObject Category -> AppM (JSONObject (Entity Category))
updateCategory categoryId (JSONObject category) = do
        let key = toSqlKey categoryId
        runDB $ replace key category
        return . JSONObject $ Entity key category

deleteCategory :: Int64 -> AppM ()
deleteCategory categoryId = void $ runDB $ delete (toSqlKey categoryId :: Key Category)
