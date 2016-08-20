module Categories.Update exposing (..)

import Categories.Messages exposing (Msg(..))
import Categories.Models exposing (Category)


update : Msg -> List Category -> ( List Category, Cmd Msg )
update msg categories =
    case msg of
        NoOp ->
            ( categories, Cmd.none )
