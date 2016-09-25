module Products.Models exposing (..)

import Dict
import Api.Models exposing (Category, Product, ProductVariant, ProductId, initialProduct)
import Models exposing (Model, UIState)


type alias ProductData =
    { products : List Product
    , productVariants : List ProductVariant
    , categories : List Category
    , showSKUs : Dict.Dict ProductId Bool
    , productForm : Product
    }


makeProductData : Model -> ProductData
makeProductData model =
    let
        ui =
            model.uiState.products
    in
        { products = model.products
        , productVariants = model.productVariants
        , categories = model.categories
        , showSKUs = ui.showSKUs
        , productForm = ui.productForm
        }
