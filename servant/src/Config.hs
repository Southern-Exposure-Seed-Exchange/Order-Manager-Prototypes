{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
module Config where


import Control.Monad.Logger                 (runNoLoggingT, runStdoutLoggingT)

import Database.Persist.Postgresql (ConnectionPool, createPostgresqlPool, ConnectionString)
import Network.Wai                          (Middleware)
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)



data Config = Config { getPool :: ConnectionPool
                     , getEnv :: Environment
                     }

data Environment = Development
                 | Test
                 | Benchmark
                 | Production
                 deriving (Eq, Show, Read)

defaultConfig :: Config
defaultConfig = Config { getPool = undefined, getEnv = Development }

-- | Enable synchronous logging in Development, async logging in
-- Production, and no logging for Tests and Benchmarks.
setLogger :: Environment -> Middleware
setLogger Test = id
setLogger Development = logStdoutDev
setLogger Production = logStdout
setLogger Benchmark = id

-- | Enable Query Logging in Development.
makePool :: Environment -> IO ConnectionPool
makePool e@Development = runStdoutLoggingT $ createPostgresqlPool (connStr e) (envPool e)
makePool e             = runNoLoggingT $ createPostgresqlPool (connStr e) (envPool e)

-- | Use multiple SQL connections in Production & when Benchmarking.
envPool :: Environment -> Int
envPool Test = 1
envPool Development = 1
envPool Production = 8
envPool Benchmark = 8

-- | Assume a passwordless connection to the `om-servant` database.
connStr :: Environment -> ConnectionString
connStr _ = "host=localhost dbname=om-servant"
