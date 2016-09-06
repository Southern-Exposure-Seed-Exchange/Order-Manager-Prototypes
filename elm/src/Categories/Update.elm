module Categories.Update exposing (..)

import Categories.Messages exposing (Msg(..))
import Categories.Models exposing (CategoryData)


update : Msg -> CategoryData -> ( CategoryData, Cmd Msg )
update msg model =
    case msg of
        FetchAllDone newModel ->
            ( newModel, Cmd.none )
        FetchAllFail _ ->
            ( model, Cmd.none )
