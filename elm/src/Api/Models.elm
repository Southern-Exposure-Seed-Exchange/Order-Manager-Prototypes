module Api.Models exposing (..)


type alias CategoryId =
    Int


type alias Category =
    { id : CategoryId
    , name : String
    , description : String
    , parent : Maybe CategoryId
    }


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
