module Categories.Models exposing (..)

import Api.Models exposing (Category, Product, initialCategory)
import Models exposing (Model, UIState(..))


type alias CategoryData =
    { categories : List Category
    , products : List Product
    , categoryForm : Category
    }


makeCategoryData : Model -> CategoryData
makeCategoryData model =
    let
        categoryForm =
            case model.uiState of
                Categories ui ->
                    ui.categoryForm
                _ ->
                    initialCategory
    in
        { categories = model.categories
        , products = model.products
        , categoryForm = categoryForm
        }
