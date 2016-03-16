{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
module Api (app) where

import Control.Monad                (liftM)
import Control.Monad.IO.Class       (MonadIO)
import Control.Monad.Reader         (ReaderT, runReaderT, lift, MonadReader, asks, liftIO)
import Control.Monad.Trans.Either   (EitherT, left)
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
readerServer cfg = enter (readerToEither cfg) server

app :: Config -> Application
app cfg = serve api $ readerServer cfg


type API = "categories"      :> CategoryAPI
      :<|> "products"        :> ProductAPI
      :<|> "productVariants" :> ProductVariantAPI
server :: ServerT API AppM
server = categoryRoutes
    :<|> productRoutes
    :<|> productVariantRoutes

type CategoryAPI = CRUD (PKey Category) Category
categoryRoutes :: CRUDRoutes (PKey Category) Category
categoryRoutes = crudRoutes

type ProductAPI = CRUD (PKey Product) Product
productRoutes :: CRUDRoutes (PKey Product) Product
productRoutes = crudRoutes

type ProductVariantAPI = CRUD (PKey ProductVariant) ProductVariant
productVariantRoutes :: CRUDRoutes (PKey ProductVariant) ProductVariant
productVariantRoutes = crudRoutes


type CRUD ident resource =
        Get '[JSON] (JSONList (Entity resource))
   :<|> ReqBody '[JSON] (JSONObject resource)
        :> Post '[JSON] (JSONObject (Entity resource))
   :<|> Capture "id" ident
        :> Get '[JSON] (JSONObject (Entity resource))
   :<|> Capture "id" ident
        :> ReqBody '[JSON] (JSONObject resource)
        :> Put '[JSON] (JSONObject (Entity resource))
   :<|> Capture "id" ident
        :> Delete '[JSON] ()

type CRUDRoutes ident resource =
         AppM (JSONList (Entity resource))
    :<|> ((JSONObject resource -> AppM (JSONObject (Entity resource)))
    :<|> ((ident -> AppM (JSONObject (Entity resource)))
    :<|> ((ident -> JSONObject resource -> AppM (JSONObject (Entity resource)))
    :<|> (ident -> AppM ()))))

crudRoutes :: (PersistEntityBackend r ~ SqlBackend, PersistEntity r,
               ToBackendKey SqlBackend r)
           => CRUDRoutes (PKey r) r
crudRoutes =    listRoute
           :<|> createRoute
           :<|> viewRoute
           :<|> updateRoute
           :<|> deleteRoute

listRoute :: (PersistEntityBackend r ~ SqlBackend, PersistEntity r)
          => AppM (JSONList (Entity r))
listRoute = liftM JSONList . runDB $ selectList [] []
createRoute :: (PersistEntityBackend r ~ SqlBackend, PersistEntity r)
          => JSONObject r -> AppM (JSONObject (Entity r))
createRoute (JSONObject item) = do
        key <- runDB $ insert item
        return . JSONObject $ Entity key item
viewRoute :: (PersistEntityBackend r ~ SqlBackend, PersistEntity r,
              ToBackendKey SqlBackend r)
          => PKey r -> AppM (JSONObject (Entity r))
viewRoute pKey =  do
        let key = _PKey pKey
        maybeItem <- runDB $ get key
        case maybeItem of
            Nothing -> lift $ left err404
            Just value -> return $ JSONObject (Entity key value)
updateRoute :: (PersistEntityBackend r ~ SqlBackend, PersistEntity r,
                ToBackendKey SqlBackend r)
            => PKey r -> JSONObject r -> AppM (JSONObject (Entity r))
updateRoute pKey (JSONObject item) = do
        let key = _PKey pKey
        runDB $ replace key item
        return . JSONObject $ Entity key item
deleteRoute :: (PersistEntityBackend r ~ SqlBackend, PersistEntity r,
                ToBackendKey SqlBackend r)
            => PKey r -> AppM ()
deleteRoute pKey = do
        let key = _PKey pKey
        maybeItem <- runDB $ get key
        case maybeItem of
            Nothing -> lift $ left err404
            _       -> runDB $ delete key
