{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Models.Base where

import Control.Monad                (mzero)
import qualified Data.Text     as T

import Data.Aeson (FromJSON(..), (.:), Value(..))
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Category json
    name T.Text
    description T.Text default=''
    parent CategoryId Maybe
    deriving Show
Product
    name T.Text
    category CategoryId
    deriving Show
|]


data CategoryList = CategoryList [Category]
instance FromJSON CategoryList where
        parseJSON (Object o) = do
            category <- o .: "category" >>= parseJSON
            return $ CategoryList [category]
        parseJSON _          = mzero
