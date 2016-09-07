module Products.Models exposing (..)

import Dict

import Api.Models exposing (Category, Product, ProductVariant, ProductId)
import Models exposing (Model, UIState(..))


type alias ProductData =
    { products : List Product
    , productVariants : List ProductVariant
    , categories : List Category
    , showSKUs : Dict.Dict ProductId Bool
    }


makeProductData : Model -> ProductData
makeProductData model =
    let
        showSKUs =
            case model.uiState of
                ProductList ui ->
                    ui.showSKUs
    in
        { products = model.products
        , productVariants = model.productVariants
        , categories = model.categories
        , showSKUs = showSKUs
        }
