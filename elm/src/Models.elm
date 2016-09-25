module Models exposing (..)

import Dict
import Api.Models exposing (Category, Product, ProductVariant, ProductId, initialProduct)
import Routing


type alias Model =
    { categories : List Category
    , products : List Product
    , productVariants : List ProductVariant
    , uiState : UIState
    , route : Routing.Route
    }


type UIState
    = Products { showSKUs : Dict.Dict ProductId Bool, productForm : Product }
    | Categories { categoryForm : Category }


initialModel : Routing.Route -> Model
initialModel route =
    { categories = []
    , products = []
    , productVariants = []
    , uiState = Products { showSKUs = Dict.empty, productForm = initialProduct }
    , route = route
    }
