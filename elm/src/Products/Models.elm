module Products.Models exposing (..)

import Dict
import Api.Models exposing (Category, Product, ProductVariant, ProductId, initialProduct)
import Models exposing (Model, UIState(..))


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
        showSKUs =
            case model.uiState of
                Products ui ->
                    ui.showSKUs

                _ ->
                    Dict.empty

        productForm =
            case model.uiState of
                Products ui ->
                    ui.productForm

                _ ->
                    initialProduct
    in
        { products = model.products
        , productVariants = model.productVariants
        , categories = model.categories
        , showSKUs = showSKUs
        , productForm = productForm
        }
