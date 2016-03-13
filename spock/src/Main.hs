{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Monad.Logger
import Control.Monad.Trans.Resource
import qualified Data.Text                     as T
import Control.Monad.IO.Class                       (MonadIO)
import Data.Aeson                                   (ToJSON(..), (.=), object)
import Database.Persist.Sqlite hiding               (get, delete)
import Network.HTTP.Types   (notFound404)
import Web.Spock.Safe
import qualified Database.Persist.Sqlite     as Sql (get, delete)

import Models.Base


main :: IO ()
main = runOM

data OMState = OMState { }
type OM a = SpockCtxT () (WebStateM SqlBackend (Maybe a) OMState) ()


runOM :: IO ()
runOM = do pool <- runNoLoggingT $ createSqlitePool "dev.sqlite" 5
           runNoLoggingT $ runSqlPool (runMigration migrateAll) pool
           runSpock 3000 $ spock (spockCfg pool) omApp
        where spockCfg pool = defaultSpockCfg Nothing (PCPool pool) OMState { }


type OMSQL a = SqlPersistT (NoLoggingT (ResourceT IO)) a
type OMSqlResult m a = (HasSpock m, SpockConn m ~ SqlBackend) => m a

runSQL :: OMSQL a -> OMSqlResult m a
runSQL action = runQuery $ \conn -> runResourceT $ runNoLoggingT $ runSqlConn action conn


type OMRoute ctx m b = (MonadIO m) => OMSqlResult (ActionCtxT ctx m) b

omApp :: OM a
omApp = subcomponent "categories" $
            do categoryListHandlers
               categoryDetailHandlers


categoryListHandlers :: OM a
categoryListHandlers = do
    get root $ do
        categories <- runSQL $ selectList [] [Asc CategoryName]
        json $ object ["categories" .= toJSON categories]
    post root $ do
        CategoryList [newCategory] <- jsonBody'
        category <- runSQL $ insertEntity newCategory
        json $ object ["categories" .= category]

categoryDetailHandlers :: OM a
categoryDetailHandlers = do
    get var $ \catId ->
            getAndWrap "categories" (toSqlKey catId :: Key Category)
    put var $ \catId ->
        let key = toSqlKey catId :: Key Category in
        getOr404 key $ \_ -> do
            CategoryList [newCategory] <- jsonBody'
            runSQL $ replace key newCategory
            json $ object ["categories" .= Entity key newCategory]
    delete var $ \catId -> do
        runSQL $ Sql.delete (toSqlKey catId :: Key Category)
        json $ object []


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
