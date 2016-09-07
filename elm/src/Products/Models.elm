module Products.Models exposing (..)

import Api.Models exposing (Category, Product, ProductVariant)


type alias ProductData =
    { products : List Product
    , productVariants : List ProductVariant
    , categories : List Category
    }
