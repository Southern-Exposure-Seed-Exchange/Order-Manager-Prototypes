module Categories.Update exposing (..)

import Navigation

import Categories.Messages exposing (Msg(..))
import Categories.Models exposing (CategoryData)
import Utils exposing (replaceBy)


update : Msg -> CategoryData -> ( CategoryData, Cmd Msg )
update msg model =
    case msg of
        FetchAllDone newModel ->
            ( newModel, Cmd.none )
        FetchAllFail _ ->
            ( model, Cmd.none )
        FetchOneDone newCategory ->
            ( { model | categories = replaceBy .id newCategory model.categories }, Cmd.none )
        FetchOneFail _ ->
            ( model, Cmd.none )
        VisitCategory id ->
            ( model, Navigation.newUrl <| "#categories/" ++ toString id )
        VisitProduct id ->
            ( model, Navigation.newUrl <| "#products/" ++ toString id )
