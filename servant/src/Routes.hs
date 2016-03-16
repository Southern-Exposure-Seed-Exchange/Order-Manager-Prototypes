{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Routes
    ( CRUD
    , CRUDRoutes
    , crudRoutes
    ) where
import Control.Monad                (liftM)
import Control.Monad.Reader         (lift)
import Control.Monad.Trans.Either   (left)
import Database.Persist.Postgresql
import Servant

import Models
import Types


type CRUD resource =
        Get '[JSON] (JSONList (Entity resource))
   :<|> ReqBody '[JSON] (JSONObject resource)
        :> Post '[JSON] (JSONObject (Entity resource))
   :<|> Capture "id" (PKey resource)
        :> Get '[JSON] (JSONObject (Entity resource))
   :<|> Capture "id" (PKey resource)
        :> ReqBody '[JSON] (JSONObject resource)
        :> Put '[JSON] (JSONObject (Entity resource))
   :<|> Capture "id" (PKey resource)
        :> Delete '[JSON] ()

type CRUDRoutes resource =
         AppM (JSONList (Entity resource))
    :<|> ((JSONObject resource -> AppM (JSONObject (Entity resource)))
    :<|> (((PKey resource) -> AppM (JSONObject (Entity resource)))
    :<|> (((PKey resource) -> JSONObject resource -> AppM (JSONObject (Entity resource)))
    :<|> ((PKey resource) -> AppM ()))))

crudRoutes :: (PersistEntityBackend r ~ SqlBackend, PersistEntity r,
               ToBackendKey SqlBackend r)
           => CRUDRoutes r
crudRoutes =    listRoute
           :<|> createRoute
           :<|> viewRoute
           :<|> updateRoute
           :<|> deleteRoute

-- | The `listRoute` returns a JSON Array of Persistent Entities.
listRoute :: (PersistEntityBackend r ~ SqlBackend, PersistEntity r)
          => AppM (JSONList (Entity r))
listRoute = liftM JSONList . runDB $ selectList [] []

-- | The `createRoute` parses a JSON request body & inserts the value into
-- the database if it is valid.
createRoute :: (PersistEntityBackend r ~ SqlBackend, PersistEntity r)
          => JSONObject r -> AppM (JSONObject (Entity r))
createRoute (JSONObject item) = do
        key <- runDB $ insert item
        return . JSONObject $ Entity key item

-- | The `viewRoute` returns a single JSON object representing a Persistent
-- Entity.
viewRoute :: (PersistEntityBackend r ~ SqlBackend, PersistEntity r,
              ToBackendKey SqlBackend r)
          => PKey r -> AppM (JSONObject (Entity r))
viewRoute pKey =  do
        let key = _PKey pKey
        maybeItem <- runDB $ get key
        case maybeItem of
            Nothing -> lift $ left err404
            Just value -> return $ JSONObject (Entity key value)

-- | The `updateRoute` attempts to update an Entity using a JSON request
-- body and returns the new Entity.
updateRoute :: (PersistEntityBackend r ~ SqlBackend, PersistEntity r,
                ToBackendKey SqlBackend r)
            => PKey r -> JSONObject r -> AppM (JSONObject (Entity r))
updateRoute pKey (JSONObject item) = do
        let key = _PKey pKey
        runDB $ replace key item
        return . JSONObject $ Entity key item

-- | The `deleteRoute` deletes the Entity, if it exists.
deleteRoute :: (PersistEntityBackend r ~ SqlBackend, PersistEntity r,
                ToBackendKey SqlBackend r)
            => PKey r -> AppM ()
deleteRoute pKey = do
        let key = _PKey pKey
        maybeItem <- runDB $ get key
        case maybeItem of
            Nothing -> lift $ left err404
            _       -> runDB $ delete key
