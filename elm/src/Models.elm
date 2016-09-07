module Models exposing (..)

import Api.Models exposing (Category, Product, ProductVariant)
import Routing


type alias Model =
    { categories : List Category
    , products : List Product
    , productVariants : List ProductVariant
    , route : Routing.Route
    }


initialModel : Routing.Route -> Model
initialModel route =
    { categories = []
    , products = []
    , productVariants = []
    , route = route
    }
