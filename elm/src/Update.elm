module Update exposing (..)

import Dict
import Navigation

import Categories.Commands
import Categories.Update
import Messages exposing (Msg(..))
import Models exposing (Model, UIState(..))
import Routing exposing (Route(..))
import Products.Commands
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
                        ( updatedModel
                        , Cmd.batch
                            [ Cmd.map CategoriesMsg Categories.Commands.fetchAll
                            , Navigation.newUrl "#categories"
                            ]
                        )
                    CategoryRoute categoryId ->
                        updatedModel !
                            [ Cmd.map CategoriesMsg (Categories.Commands.fetchOne categoryId)
                            , Navigation.newUrl <| "#categories" ++ toString categoryId
                            ]
                    ProductsRoute ->
                        updatedModel !
                            [ Cmd.map ProductsMsg Products.Commands.fetchAll
                            , Navigation.newUrl "#products"
                            ]
                    NotFoundRoute ->
                        ( updatedModel, Cmd.none )
        CategoriesMsg subMsg ->
            let
                ( updatedModel, cmd ) =
                    Categories.Update.update subMsg
                        { categories = model.categories, products = model.products }
            in
               ( { model | categories = updatedModel.categories
                         , products = updatedModel.products }
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
