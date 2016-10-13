{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
-- | This script imports previous data from the Stone Edge Order Manager
module DataMigration where

import Control.Monad                (liftM)
import Control.Monad.IO.Class       (MonadIO)
import Control.Monad.Reader         (ReaderT)
import Data.Csv                     (decode, HasHeader(..), FromRecord)
import Data.Foldable                (toList)
import Data.Function                (on)
import Data.Int                     (Int64)
import Data.List                    (nub, nubBy)
import Data.Maybe                   (fromJust)
import Database.Persist.Postgresql
import GHC.Generics                 (Generic)
import System.Environment           (lookupEnv)
import Text.Read                    (readMaybe)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text            as T

import Config                       (Environment(..), makePool)
import qualified Models          as M


type SQL m b = (MonadIO m) => ReaderT SqlBackend m b

main :: IO ()
main = do
    env <- lookupSetting "ENV" Development
    pool <- makePool env
    csvText <- BS.readFile "./OM_Products_Export.csv"
    let result = toList <$> decode HasHeader csvText
    case result of
        Left s   -> print s
        Right ps -> flip runSqlPool pool $ do
            M.doMigrations >> dropDatabase
            mapM_ insert (map catToDB . nub $ map category ps :: [M.Category])
            (dbProducts :: [M.Product]) <- nubBy ((==) `on` M.productName)
                                       <$> mapM prodToDB ps
            mapM_ insert dbProducts
            dbVariants <- mapM prodToVariant ps
            mapM_ insert dbVariants


lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
        p <- lookupEnv env
        return $ case p of Nothing -> def
                           Just a -> read a



dropDatabase :: SQL IO ()
dropDatabase = do
    (pvs :: [Entity M.ProductVariant]) <- selectList [] []
    deleteWhere [M.ProductVariantId  <-. entityIds pvs]
    (ps :: [Entity M.Product]) <- selectList [] []
    deleteWhere [M.ProductId <-. entityIds ps]
    (cs :: [Entity M.Category]) <- selectList [] []
    deleteWhere [M.CategoryId <-. entityIds cs]
    where entityIds = map (\(Entity i _) -> i)


data CSVProduct = CSVProduct { sku :: !T.Text
                             , name :: !T.Text
                             , quantity :: Maybe Int64
                             , price :: !T.Text
                             , weight :: Maybe Double
                             , active :: !T.Text
                             , contents :: !T.Text
                             , heirloom :: !T.Text
                             , organic :: !T.Text
                             , category :: !T.Text
                             } deriving Generic
instance FromRecord CSVProduct


catToDB :: T.Text -> M.Category
catToDB t = M.Category t "" Nothing

prodToDB :: CSVProduct -> SQL m M.Product
prodToDB csvp = do
    let catName = category csvp
    (Entity catId _ :: Entity M.Category) <- fmap fromJust . getBy $ M.UniqueCategory catName
    return $ M.Product (name csvp)
                       (contents csvp)
                       catId
                       (fromTextBool $ organic csvp)
                       (fromTextBool $ heirloom csvp)
                       False
                       True
    where fromTextBool "True" = True
          fromTextBool _      = False

prodToVariant :: CSVProduct -> SQL m M.ProductVariant
prodToVariant csvp = do
        let prodName = name csvp
        (Entity prodId _ :: Entity M.Product) <- fmap fromJust . getBy $ M.UniqueProduct prodName
        return $ M.ProductVariant prodId
                                  (sku csvp)
                                  (round . maybeCenti $ weight csvp)
                                  (readCenti . T.unpack . stripDollar $ price csvp)
                                  (maybeCenti $ quantity csvp)
        where maybeCenti :: (Num a) => Maybe a -> a
              maybeCenti  = maybe 0 (* 100)
              stripDollar = T.replace "$" ""
              readCenti   = maybeCenti . readMaybe
