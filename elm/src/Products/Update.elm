module Products.Update exposing (..)

import Products.Messages exposing (Msg(..))
import Products.Models exposing (ProductData)


update : Msg -> ProductData -> ( ProductData, Cmd Msg )
update msg model =
    case msg of
        FetchAllDone newModel ->
            ( newModel, Cmd.none )
        FetchAllFail _ ->
            ( model, Cmd.none )
