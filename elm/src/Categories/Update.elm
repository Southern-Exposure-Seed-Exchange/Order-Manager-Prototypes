module Categories.Update exposing (..)

import Navigation

import Categories.Messages exposing (Msg(..))
import Categories.Models exposing (CategoryData)
import Utils exposing (replaceAllById)


update : Msg -> CategoryData -> ( CategoryData, Cmd Msg )
update msg model =
    case msg of
        FetchAllDone newModel ->
            ( newModel, Cmd.none )
        FetchAllFail _ ->
            ( model, Cmd.none )
        FetchOneDone newModel ->
            ( updateModel model newModel, Cmd.none )
        FetchOneFail _ ->
            ( model, Cmd.none )
        VisitCategory id ->
            ( model, Navigation.newUrl <| "#categories/" ++ toString id )
        VisitProduct id ->
            ( model, Navigation.newUrl <| "#products/" ++ toString id )


updateModel : CategoryData -> CategoryData -> CategoryData
updateModel model newData =
    let
        updateAttribute =
            replaceAllById model newData
    in
        { model
            | categories = updateAttribute .categories
            , products = updateAttribute .products }
