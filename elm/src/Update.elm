module Update exposing (..)

import Categories.Update
import Messages exposing (Msg(..))
import Models exposing (Model)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CategoriesMsg subMsg ->
            let
                ( updatedModel, cmd ) =
                    Categories.Update.update subMsg model
            in
               ( updatedModel, Cmd.map CategoriesMsg cmd )
