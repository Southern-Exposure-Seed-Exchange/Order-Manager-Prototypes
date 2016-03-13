module Main where

import Control.Monad.Logger
import Database.Persist.Sqlite               hiding (get, delete)
import Web.Spock.Safe

import App          (omApp)
import Types
import Models.Base (migrateAll)

main :: IO ()
main = runOM

runOM :: IO ()
runOM = do
    pool <- runNoLoggingT $ createSqlitePool "dev.sqlite" 5
    runNoLoggingT $ runSqlPool (runMigration migrateAll) pool
    runSpock 3000 $ spock (spockCfg pool) omApp
    where spockCfg pool = defaultSpockCfg Nothing (PCPool pool) OMState { }
