module Update exposing (..)

import Categories.Models exposing (makeCategoryData)
import Categories.Update
import Messages exposing (Msg(..))
import Models exposing (Model, UIState(..))
import NavBar
import Products.Models exposing (makeProductData)
import Products.Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NavBarMessage subMsg ->
            NavBar.update subMsg model
                |> (\( m, c ) -> ( m, Cmd.map NavBarMessage c ))

        CategoriesMsg subMsg ->
            let
                ( updatedModel, cmd ) =
                    makeCategoryData model
                        |> Categories.Update.update subMsg

                updatedUI =
                    Categories { categoryForm = updatedModel.categoryForm }
            in
                ( { model
                    | categories = updatedModel.categories
                    , products = updatedModel.products
                    , uiState = updatedUI
                  }
                , Cmd.map CategoriesMsg cmd
                )

        ProductsMsg subMsg ->
            let
                ( updatedModel, cmd ) =
                    makeProductData model
                        |> Products.Update.update subMsg

                updatedUI =
                    Products
                        { showSKUs = updatedModel.showSKUs
                        , productForm = updatedModel.productForm
                        }
            in
                ( { model
                    | products = updatedModel.products
                    , productVariants = updatedModel.productVariants
                    , categories = updatedModel.categories
                    , uiState = updatedUI
                  }
                , Cmd.map ProductsMsg cmd
                )
