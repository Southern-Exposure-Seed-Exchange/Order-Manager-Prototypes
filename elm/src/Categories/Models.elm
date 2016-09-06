module Categories.Models exposing (..)


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

type alias CategoryData =
    { categories : List Category
    , products : List Product
    }


new : Category
new = 
    { id = 0
    , name = ""
    , description = ""
    , parent = Nothing
    }
