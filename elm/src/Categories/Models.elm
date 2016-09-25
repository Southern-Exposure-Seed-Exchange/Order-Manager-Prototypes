module Categories.Models exposing (..)

import Api.Models exposing (Category, Product, initialCategory)
import Models exposing (Model)


type alias CategoryData =
    { categories : List Category
    , products : List Product
    , categoryForm : Category
    }


makeCategoryData : Model -> CategoryData
makeCategoryData model =
    { categories = model.categories
    , products = model.products
    , categoryForm = model.uiState.categories.categoryForm
    }
