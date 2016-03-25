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
import Control.Monad.Trans.Either   (left)
import Database.Persist.Postgresql
import Servant

import Models
import Types
import Validation                   (Validation(..))


type CRUD resource =
        Get '[JSON] (Sideloaded (Entity resource))
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
         AppM (Sideloaded (Entity resource))
    :<|> ((JSONObject resource -> AppM (JSONObject (Entity resource)))
    :<|> ((PKey resource -> AppM (JSONObject (Entity resource)))
    :<|> ((PKey resource -> JSONObject resource -> AppM (JSONObject (Entity resource)))
    :<|> (PKey resource -> AppM ()))))

crudRoutes :: (PersistEntityBackend r ~ SqlBackend, PersistEntity r,
               ToBackendKey SqlBackend r, Sideload (Entity r), Validation r)
           => CRUDRoutes r
crudRoutes =    listRoute
           :<|> createRoute
           :<|> viewRoute
           :<|> updateRoute
           :<|> deleteRoute

-- | The `listRoute` returns a JSON Array of Persistent Entities, along
-- with any sideloaded Entities.
listRoute :: (PersistEntityBackend r ~ SqlBackend, PersistEntity r,
              Sideload (Entity r))
          => AppM (Sideloaded (Entity r))
listRoute = runDB (selectList [] []) >>= sideload

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
                ToBackendKey SqlBackend r, Validation r)
            => PKey r -> JSONObject r -> AppM (JSONObject (Entity r))
updateRoute pKey (JSONObject item) = do
        let key = _PKey pKey
        validated <- runValidate (Just key) item
        runDB $ replace key validated
        return . JSONObject $ Entity key validated

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
