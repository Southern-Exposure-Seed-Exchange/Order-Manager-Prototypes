module Categories.Models exposing (..)

import Api.Models exposing (Category, Product, initialCategory)
import Models exposing (Model)
import Categories.Form exposing (FormErrors, initialErrors)


type alias CategoryData =
    { categories : List Category
    , products : List Product
    , categoryForm : Category
    , formErrors : FormErrors
    }


makeCategoryData : Model -> CategoryData
makeCategoryData model =
    { categories = model.categories
    , products = model.products
    , categoryForm = model.uiState.categories.categoryForm
    , formErrors = model.uiState.categories.formErrors
    }
