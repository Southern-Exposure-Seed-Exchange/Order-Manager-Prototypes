module Api.Models where

import Prelude
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.?))
import Data.Maybe (Maybe)


type CategoryId = Int


data Category = Category
    { id :: CategoryId
    , name :: String
    , description :: String
    , parent :: Maybe CategoryId
    }


instance decodeJsonCategory :: DecodeJson Category where
    decodeJson json = do
        obj <- decodeJson json
        id <- obj .? "id"
        name <- obj .? "name"
        description <- obj .? "description"
        parent <- obj .? "parent"
        pure $ Category { id, name, description, parent }


type ProductId = Int


data Product = Product
    { id :: ProductId
    , name :: String
    , description :: String
    , category :: CategoryId
    , isActive :: Boolean
    , isOrganic :: Boolean
    , isHeirloom :: Boolean
    , isSouthEast :: Boolean
    }


instance decodeJsonProduct :: DecodeJson Product where
    decodeJson json = do
        obj <- decodeJson json
        id <- obj .? "id"
        name <- obj .? "name"
        description <- obj .? "description"
        category <- obj .? "category"
        isActive <- obj .? "isActive"
        isOrganic <- obj .? "isOrganic"
        isHeirloom <- obj .? "isHeirloom"
        isSouthEast <- obj .? "isSouthEast"
        pure $ Product { id, name, description, category
                       , isActive, isOrganic, isHeirloom, isSouthEast }
