module Categories.Models exposing (..)


type alias CategoryId =
    Int


type alias Category =
    { id : CategoryId
    , name : String
    , description : String
    , parent : Maybe CategoryId
    }


new : Category
new = 
    { id = 0
    , name = ""
    , description = ""
    , parent = Nothing
    }
