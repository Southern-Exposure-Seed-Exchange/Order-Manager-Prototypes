module Models exposing (..)

import Dict

import Api.Models exposing (Category, Product, ProductVariant, ProductId)
import Routing


type alias Model =
    { categories : List Category
    , products : List Product
    , productVariants : List ProductVariant
    , uiState : UIState
    , route : Routing.Route
    }


type UIState
    = ProductList { showSKUs : Dict.Dict ProductId Bool }
    | Categories { categoryForm : Category }


initialModel : Routing.Route -> Model
initialModel route =
    { categories = []
    , products = []
    , productVariants = []
    , uiState = ProductList { showSKUs = Dict.empty }
    , route = route
    }
