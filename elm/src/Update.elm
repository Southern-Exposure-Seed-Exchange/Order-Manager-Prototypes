module Update exposing (..)

import Categories.Models exposing (CategoryData)
import Categories.Update
import Messages exposing (Msg(..))
import Models exposing (Model)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CategoriesMsg subMsg ->
            let
                ( updatedModel, cmd ) =
                    Categories.Update.update subMsg
                        { categories = model.categories, products = model.products }
            in
               ( { model | categories = updatedModel.categories
                         , products = updatedModel.products }
               , Cmd.map CategoriesMsg cmd )
