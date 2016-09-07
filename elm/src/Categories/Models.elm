module Categories.Models exposing (..)

import Api.Models exposing (Category, Product)


type alias CategoryData =
    { categories : List Category
    , products : List Product
    }
