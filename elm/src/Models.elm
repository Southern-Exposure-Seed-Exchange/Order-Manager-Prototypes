module Models exposing (..)

import Dict
import Api.Models exposing (Category, Product, ProductVariant, ProductId, initialProduct, initialCategory)
import Categories.Form
import Products.Form
import Routing


type alias Model =
    { categories : List Category
    , products : List Product
    , productVariants : List ProductVariant
    , uiState : UIState
    , route : Routing.Route
    }


type alias UIState =
    { products :
        { showSKUs : Dict.Dict ProductId Bool
        , productForm : Product
        , formErrors : Products.Form.FormErrors
        }
    , categories :
        { categoryForm : Category
        , formErrors : Categories.Form.FormErrors
        }
    }


initialUIState : UIState
initialUIState =
    { products =
        { showSKUs = Dict.empty
        , productForm = initialProduct
        , formErrors = Products.Form.initialErrors
        }
    , categories =
        { categoryForm = initialCategory
        , formErrors = Categories.Form.initialErrors
        }
    }


initialModel : Routing.Route -> Model
initialModel route =
    { categories = []
    , products = []
    , productVariants = []
    , uiState = initialUIState
    , route = route
    }
