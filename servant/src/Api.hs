{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Api where

import Control.Monad.Reader         (runReaderT)
import Control.Monad.Trans.Except   (ExceptT)
import Network.Wai                  (Application)
import Servant

import Config       (Config(..))
import Models
import Routes
import Types


app :: Config -> Application
app cfg = serve api $ readerServer cfg

api :: Proxy API
api = Proxy

readerServer :: Config -> Server API
readerServer cfg = enter (readerToEither cfg) server

readerToEither :: Config -> AppM :~> ExceptT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg

type API = "categories"      :> CategoryAPI
      :<|> "products"        :> ProductAPI
      :<|> "productVariants" :> ProductVariantAPI
server :: ServerT API AppM
server = categoryRoutes
    :<|> productRoutes
    :<|> productVariantRoutes


type CategoryAPI = CRUD Category
categoryRoutes :: CRUDRoutes Category
categoryRoutes = crudRoutes

type ProductAPI = CRUD Product
productRoutes :: CRUDRoutes Product
productRoutes = crudRoutes

type ProductVariantAPI = CRUD ProductVariant
productVariantRoutes :: CRUDRoutes ProductVariant
productVariantRoutes = crudRoutes
