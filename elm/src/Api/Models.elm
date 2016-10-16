module Api.Models exposing (..)


type alias ApiError =
    { source : String
    , detail : String
    }


type alias CategoryId =
    Int


type alias Category =
    { id : CategoryId
    , name : String
    , description : String
    , parent : Maybe CategoryId
    }


initialCategory : Category
initialCategory =
    { id = 0, name = "", description = "", parent = Nothing }


type alias ProductId =
    Int


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


type alias ProductVariantId =
    Int


type alias ProductVariant =
    { id : ProductVariantId
    , sku : String
    , product : ProductId
    , price : Int
    , quantity : Int
    , weight : Int
    }
