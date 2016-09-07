module Update exposing (..)

import Navigation

import Categories.Commands
import Categories.Models exposing (CategoryData)
import Categories.Update
import Messages exposing (Msg(..))
import Models exposing (Model)
import Routing exposing (Route(..))


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
