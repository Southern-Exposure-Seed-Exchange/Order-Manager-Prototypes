{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Routes
    ( CRUD
    , CRUDRoutes
    , crudRoutes
    ) where
import Control.Monad.Reader         (lift)
import Control.Monad.Trans.Except   (throwE)
import Database.Persist.Postgresql
import Servant

import Models
import Types
import Validation                   (Validation(..))

-- | The URL schema for standard JSON REST routes - List/Create routes for
-- GET/POST requests to the root URL, View/Update/Delete routes for
-- GET/PUT/DELETE requests to the root URL plus the ID.
type CRUD resource =
        Get '[JSON] (Sideloaded (Entity resource))
   :<|> ReqBody '[JSON] (JSONObject resource)
        :> Post '[JSON] (JSONObject (Entity resource))
   :<|> Capture "id" (PKey resource)
        :> Get '[JSON] (Sideloaded (Entity resource))
   :<|> Capture "id" (PKey resource)
        :> ReqBody '[JSON] (JSONObject resource)
        :> Put '[JSON] (JSONObject (Entity resource))
   :<|> Capture "id" (PKey resource)
        :> Delete '[JSON] ()

-- | CRUDRoutes represent the classic REST routes for a Model - List,
-- Create, View, Update, & Delete.
type CRUDRoutes resource =
         AppM (Sideloaded (Entity resource))
    :<|> ((JSONObject resource -> AppM (JSONObject (Entity resource)))
    :<|> ((PKey resource -> AppM (Sideloaded (Entity resource)))
    :<|> ((PKey resource -> JSONObject resource -> AppM (JSONObject (Entity resource)))
    :<|> (PKey resource -> AppM ()))))

-- | A generalized set of REST routes - List, Create, View, Update,
-- & Delete - relying on the `Sideload`, `Validation`, and
-- `DefaultOrdering` type classes to generate the routes.
crudRoutes :: (PersistEntityBackend r ~ SqlBackend, ToBackendKey SqlBackend r,
               Sideload (Entity r), Validation r, DefaultOrdering r)
           => CRUDRoutes r
crudRoutes =    listRoute
           :<|> createRoute
           :<|> viewRoute
           :<|> updateRoute
           :<|> deleteRoute

-- | The `listRoute` returns a JSON Array of Persistent Entities, along
-- with any sideloaded Entities.
listRoute :: (PersistEntityBackend r ~ SqlBackend, PersistEntity r,
              Sideload (Entity r), DefaultOrdering r)
          => AppM (Sideloaded (Entity r))
listRoute = runDB (selectList [] [defaultOrdering]) >>= sideload

-- | The `createRoute` parses a JSON request body & inserts the value into
-- the database if it is valid.
createRoute :: (PersistEntityBackend r ~ SqlBackend, PersistEntity r, Validation r)
          => JSONObject r -> AppM (JSONObject (Entity r))
createRoute (JSONObject item) = do
        validated <- runValidate Nothing item
        key <- runDB $ insert validated
        return . JSONObject $ Entity key validated

-- | The `viewRoute` returns a single JSON object representing a Persistent
-- Entity.
viewRoute :: (PersistEntityBackend r ~ SqlBackend, ToBackendKey SqlBackend r,
              Sideload (Entity r))
          => PKey r -> AppM (Sideloaded (Entity r))
viewRoute pKey =  do
        let key = _PKey pKey
        maybeItem <- runDB $ get key
        case maybeItem of
            Nothing -> lift $ throwE err404
            Just value -> sideload [Entity key value]

-- | The `updateRoute` attempts to update an Entity using a JSON request
-- body and returns the new Entity.
updateRoute :: (PersistEntityBackend r ~ SqlBackend, ToBackendKey SqlBackend r,
                Validation r)
            => PKey r -> JSONObject r -> AppM (JSONObject (Entity r))
updateRoute pKey (JSONObject item) = do
        let key = _PKey pKey
        validated <- runValidate (Just key) item
        runDB $ replace key validated
        return . JSONObject $ Entity key validated

-- | The `deleteRoute` deletes the Entity, if it exists.
deleteRoute :: (PersistEntityBackend r ~ SqlBackend, ToBackendKey SqlBackend r)
            => PKey r -> AppM ()
deleteRoute pKey = do
        let key = _PKey pKey
        maybeItem <- runDB $ get key
        case maybeItem of
            Nothing -> lift $ throwE err404
            _       -> runDB $ delete key
