module Main where

import Database.Persist.Postgresql  (runSqlPool)
import Network.Wai.Handler.Warp     (run)
import System.Environment           (lookupEnv)

import Api                          (app)
import Config                       ( defaultConfig, Config(..), Environment(..)
                                    , setLogger, makePool)
import Docs                         (docsApp)
import Models                       (doMigrations)


main :: IO ()
main = do
    env <- lookupSetting "ENV" Development
    port <- lookupSetting "PORT" 3000
    pool <- makePool env
    let cfg = defaultConfig { getPool = pool, getEnv = env }
        logger = setLogger env
        server = configureServer env cfg
    runSqlPool doMigrations pool
    run port $ logger server
    where configureServer Development = docsApp
          configureServer _           = app

lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
        p <- lookupEnv env
        return $ case p of Nothing -> def
                           Just a -> read a
