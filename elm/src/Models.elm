module Models exposing (..)

import Dict
import Api.Models exposing (Category, Product, ProductVariant, ProductId, initialProduct, initialCategory)
import Routing


type alias Model =
    { categories : List Category
    , products : List Product
    , productVariants : List ProductVariant
    , uiState : UIState
    , route : Routing.Route
    }


type alias UIState =
    { products : { showSKUs : Dict.Dict ProductId Bool, productForm : Product }
    , categories : { categoryForm : Category }
    }


initialUIState : UIState
initialUIState =
    { products = { showSKUs = Dict.empty, productForm = initialProduct }
    , categories = { categoryForm = initialCategory }
    }


initialModel : Routing.Route -> Model
initialModel route =
    { categories = []
    , products = []
    , productVariants = []
    , uiState = initialUIState
    , route = route
    }
