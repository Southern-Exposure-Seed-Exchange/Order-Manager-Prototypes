module Api.Models exposing (..)

{-| This module contains types that describe the data received from the backend
server, as well as initial values for the types.

# Definitions

@docs Category, CategoryId, Product, ProductId, ProductVariant, ProductVariantId

# Initial Values

@docs initialCategory, initialProduct

-}


{-| A `Category` is identified by a unique `Int`.
-}
type alias CategoryId =
    Int


{-| A `Category` is used to create a hierarchy for organizing `Products`.
-}
type alias Category =
    { id : CategoryId
    , name : String
    , description : String
    , parent : Maybe CategoryId
    }


{-| Initial Categories have an `id` of `0` and blank fields.
-}
initialCategory : Category
initialCategory =
    { id = 0, name = "", description = "", parent = Nothing }


{-| A `Product` is identified by a unique `Int`.
-}
type alias ProductId =
    Int


{-| A `Product` describes a type of item that can be purchased. Products store
general information about an item and group together `ProductVariants` which
contain specific information like the price and weight.
-}
type alias Product =
    { id : ProductId
    , name : String
    , description : String
    , category : CategoryId
    , isActive : Bool
    , isOrganic : Bool
    , isHeirloom : Bool
    , isSouthEast : Bool
    }


{-| Initial Products have an `id` of `0`, blank fields, and are marked as
active.
-}
initialProduct : Product
initialProduct =
    { id = 0
    , name = ""
    , description = ""
    , category = 0
    , isActive = True
    , isOrganic = False
    , isHeirloom = False
    , isSouthEast = False
    }


{-| A `ProductVariant` is identified by a unique `Int`.
-}
type alias ProductVariantId =
    Int


{-| A `ProductVariant` describes a specific `Product` that is sold.
-}
type alias ProductVariant =
    { id : ProductVariantId
    , sku : String
    , product : ProductId
    , price : Int
    , quantity : Int
    , weight : Int
    }
