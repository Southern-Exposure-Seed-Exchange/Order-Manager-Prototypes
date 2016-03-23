{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | This script imports previous data from the Stone Edge Order Mangaer
module DataMigration where

import Control.Monad                (liftM)
import Control.Monad.IO.Class       (MonadIO)
import Control.Monad.Reader         (ReaderT)
import Data.Csv                     (decode, HasHeader(..), FromRecord)
import Data.Foldable                (toList)
import Data.Function                (on)
import Data.List                    (nub, nubBy)
import Data.Maybe                   (fromJust)
import Data.Text                    (Text)
import Database.Persist.Postgresql  (runSqlPool, insert, Entity(..), getBy,
                                     SqlBackend)
import GHC.Generics                 (Generic)
import System.Environment           (lookupEnv)
import qualified Data.ByteString.Lazy as BS

import Config                       (Environment(..), makePool)
import qualified Models             as M


main :: IO ()
main = do
    env <- lookupSetting "ENV" Development
    pool <- makePool env
    runSqlPool M.doMigrations pool
    csvText <- BS.readFile "./OM_Products_Export.csv"
    let result = toList <$> decode HasHeader csvText
    case result of
        Left s   -> print s
        Right ps -> flip runSqlPool pool $ do
            let categories = nub $ map category ps
                dbCategories = map catToDB categories :: [M.Category]
            mapM_ insert dbCategories
            (dbProducts :: [M.Product]) <- liftM (nubBy ((==) `on` M.productName))
                                         $ mapM prodToDB ps
            mapM_ insert dbProducts


lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
        p <- lookupEnv env
        return $ case p of Nothing -> def
                           Just a -> read a




data CSVProduct = CSVProduct { sku :: !Text
                             , name :: !Text
                             , quantity :: Maybe Int
                             , price :: Maybe Text
                             , weight :: Maybe Double
                             , active :: !Text
                             , contents :: !Text
                             , heirloom :: !Text
                             , organic :: !Text
                             , category :: !Text
                             } deriving Generic
instance FromRecord CSVProduct


catToDB :: Text -> M.Category
catToDB t = M.Category t "" Nothing

prodToDB :: (MonadIO m) => CSVProduct -> ReaderT SqlBackend m M.Product
prodToDB csvp = do
    let catName = category csvp
    (Entity catId _) <- liftM fromJust . getBy $ M.UniqueCategory catName
    return $ M.Product (name csvp)
                       (contents csvp)
                       catId
                       (fromTextBool $ organic csvp)
                       (fromTextBool $ heirloom csvp)
                       False
                       True
    where fromTextBool "True" = True
          fromTextBool _      = False
