module Update exposing (..)

import Navigation

import Categories.Models exposing (makeCategoryData)
import Categories.Update
import Messages exposing (Msg(..))
import Models exposing (Model, UIState(..))
import Routing exposing (Route(..))
import Products.Models exposing (makeProductData)
import Products.Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RoutingMsg route ->
            let
                updatedModel = { model | route = route }
            in
                case route of
                    DashboardRoute ->
                        ( updatedModel, Navigation.newUrl "#dashboard" )
                    CategoriesRoute ->
                        ( updatedModel,  Navigation.newUrl "#categories" )
                    CategoryAddRoute ->
                        ( updatedModel, Navigation.newUrl "#categories/add" )
                    CategoryRoute categoryId ->
                        ( updatedModel
                        , Navigation.newUrl <| "#categories/" ++ toString categoryId )
                    CategoryEditRoute categoryId ->
                        ( updatedModel
                        , Navigation.newUrl <| "#categories/" ++ toString categoryId ++ "/edit" )
                    ProductsRoute ->
                        ( updatedModel, Navigation.newUrl "#products" )
                    ProductRoute productId ->
                        ( updatedModel
                        , Navigation.newUrl <| "#products/" ++ toString productId )
                    NotFoundRoute ->
                        ( updatedModel, Cmd.none )
        CategoriesMsg subMsg ->
            let
                ( updatedModel, cmd ) =
                    makeCategoryData model
                        |> Categories.Update.update subMsg
                updatedUI =
                    Categories { categoryForm = updatedModel.categoryForm }

            in
               ( { model | categories = updatedModel.categories
                         , products = updatedModel.products
                         , uiState = updatedUI }
               , Cmd.map CategoriesMsg cmd )
        ProductsMsg subMsg ->
            let
                ( updatedModel, cmd ) =
                    makeProductData model
                        |> Products.Update.update subMsg
                updatedUI =
                    ProductList { showSKUs = updatedModel.showSKUs }
            in
               ( { model | products = updatedModel.products, productVariants = updatedModel.productVariants
                 , categories = updatedModel.categories, uiState = updatedUI }
               , Cmd.map ProductsMsg cmd )
