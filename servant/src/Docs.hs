{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Docs (docsApp) where

import Data.Aeson                   (toJSON)
import Data.ByteString.Lazy         (ByteString)
import Data.Monoid                  ((<>))
import Data.Text.Lazy               (pack, toStrict, fromStrict)
import Data.Text.Lazy.Encoding      (encodeUtf8)
import Database.Persist             (Entity(..))
import Database.Persist.Sql         (toSqlKey)
import Network.HTTP.Types.Status    (ok200)
import Network.Wai                  (Application, responseLBS)
import Servant
import Servant.Docs
import CMark                        (commonmarkToHtml)

import Api
import Config
import Models
import Types


type DocsAPI = Api.API :<|> ("docs" :> Raw)

docsApp :: Config -> Application
docsApp cfg = serve docsApi $ docsReaderServer cfg

docsApi :: Proxy DocsAPI
docsApi = Proxy

docsReaderServer :: Config -> Server DocsAPI
docsReaderServer cfg = enter (readerToEither cfg) Api.server :<|> serveDocs
        where serveDocs _ respond =
                respond $ responseLBS ok200 [("Content-Type", "text/html")] docsBS

docsBS :: ByteString
docsBS = encodeUtf8 . renderMarkdown . pack . markdown . docsWithIntros [intro] $ pretty api
    where renderMarkdown = fromStrict . commonmarkToHtml [] . toStrict
          intro = DocIntro "OM Backend Docs"
            [ "This is the Servant REST API Documentation for OM, the " <>
              "Enlightened Order Manager."
            , "Enjoy!"
            ]

instance ToSample (()) where
        toSamples _ = noSamples


-- Path Documentation

instance ToCapture (Capture "id" (PKey Product)) where
        toCapture _ =
            DocCapture "id"
                       "(integer) primary key of a Product"
instance ToCapture (Capture "id" (PKey Category)) where
        toCapture _ =
            DocCapture "id"
                       "(integer) primary key of a Category"
instance ToCapture (Capture "id" (PKey ProductVariant)) where
        toCapture _ =
            DocCapture "id"
                       "(integer) primary key of a Product Variant"


-- API Examples

cat1 :: Category
cat1 = Category  "TV Shows" "Stuff thats around X-Files quality." Nothing
cat2 :: Category
cat2 = Category  "Vegetables" "Tables that like to vege." (Just $ toSqlKey 99)
instance ToSample (JSONObject (Entity Category)) where
        toSamples _ = singleSample . JSONObject $ Entity (toSqlKey 42) cat1
instance ToSample (JSONObject Category) where
        toSamples _ = singleSample $ JSONObject cat1
instance ToSample (Sideloaded (Entity Category)) where
        toSamples _ = singleSample $ Sideloaded
            ( JSONList [Entity (toSqlKey 42) cat1, Entity (toSqlKey 96) cat2]
            , toJSON $ JSONList [Entity (toSqlKey 22) prod1, Entity (toSqlKey 11) prod2]
            )

prod1 :: Product
prod1 = Product "Scully's Fire Peppers" "They look like little red alian babys."
               (toSqlKey 96) True False True True
prod2 :: Product
prod2 = Product "Mulder's Mad Mushrooms" "Eat enough and everyone will call you 'Spooky'."
               (toSqlKey 96) True False True True
instance ToSample (JSONObject (Entity Product)) where
        toSamples _ = singleSample . JSONObject $ Entity (toSqlKey 42) prod1
instance ToSample (JSONObject Product) where
        toSamples _ = singleSample $ JSONObject prod1
instance ToSample (Sideloaded (Entity Product)) where
        toSamples _ = singleSample $ Sideloaded
            ( JSONList [Entity (toSqlKey 42) prod1, Entity (toSqlKey 89) prod2]
            , toJSON $ JSONList [Entity (toSqlKey 96) cat1]
            )

variant1 :: ProductVariant
variant1 = ProductVariant (toSqlKey 42) "1001A" 2500 250 30
variant2 :: ProductVariant
variant2 = ProductVariant (toSqlKey 24) "4224C" 125 750 20
instance ToSample (JSONObject ProductVariant) where
        toSamples _ = singleSample $ JSONObject variant1
instance ToSample (JSONObject (Entity ProductVariant)) where
        toSamples _ = singleSample . JSONObject $ Entity (toSqlKey 69) variant1
instance ToSample (Sideloaded (Entity ProductVariant)) where
        toSamples _ = singleSample $ Sideloaded
            ( JSONList [Entity (toSqlKey 77) variant1, Entity (toSqlKey 33) variant2]
            , toJSON $ JSONList [Entity (toSqlKey 42) prod1, Entity (toSqlKey 24) prod2]
            )
