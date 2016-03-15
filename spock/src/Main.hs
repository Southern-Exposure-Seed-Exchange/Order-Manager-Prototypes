module Main where

import Control.Monad.Logger
import Database.Persist.Sqlite               hiding (get, delete)
import Web.Spock.Safe
import Network.Wai.Middleware.RequestLogger

import App          (omApp)
import Types
import Models.Base (migrateAll)

main :: IO ()
main = runOM $ middleware logStdoutDev

develMain :: IO ()
develMain = runOM $ middleware logStdoutDev

runOM :: OM a -> IO ()
runOM withMiddleware = do
    pool <- runNoLoggingT $ createSqlitePool "dev.sqlite" 5
    runNoLoggingT $ runSqlPool (runMigration migrateAll) pool
    runSpock 3000 $ spock (spockCfg pool) (withMiddleware >> omApp)
    where spockCfg pool = defaultSpockCfg Nothing (PCPool pool) OMState { }
