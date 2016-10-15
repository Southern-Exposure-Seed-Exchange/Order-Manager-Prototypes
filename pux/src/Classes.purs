module Classes where

import Model (Model)
import Api.Models (Category(..), Product(..), ProductVariant(..))


-- | The `SubModel` typeclass is used to convert the global `Model` into a
-- | datatype with a subset of the global fields, and to update the global
-- | `Model` with the subset's data.
class SubModel a where
    updateModel :: Model -> a -> Model
    fromModel :: Model -> a


-- | The `HasId` typeclass is used to pull an `Int` `id` field out of a
-- | datatype.
class HasId a where
    toId :: a -> Int


instance hasIdCategory :: HasId Category where
    toId (Category c) = c.id


instance hasIdProduct :: HasId Product where
    toId (Product p) = p.id


instance hasIdProductVariant :: HasId ProductVariant where
    toId (ProductVariant pv) = pv.id
