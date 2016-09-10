module Categories.Update exposing (..)

import Navigation

import Api.Models exposing (Category)
import Categories.Commands exposing (fetchOne)
import Categories.Messages exposing (Msg(..))
import Categories.Models exposing (CategoryData)


update : Msg -> CategoryData -> ( CategoryData, Cmd Msg )
update msg model =
    case msg of
        FetchAllDone newModel ->
            ( newModel, Cmd.none )
        FetchAllFail _ ->
            ( model, Cmd.none )
        FetchOneDone newCategory ->
            ( updateModel model newCategory, Cmd.none )
        FetchOneFail _ ->
            ( model, Cmd.none )
        VisitCategory categoryId ->
            model ! [ fetchOne categoryId
                    , Navigation.newUrl <| "#categories/" ++ toString categoryId
                    ]


updateModel : CategoryData -> Category -> CategoryData
updateModel model category =
    let
        replace list =
            case list of
                [] ->
                    [ category ]
                c::cs ->
                    if c.id == category.id
                    then category :: cs
                    else c :: replace cs
    in
       { model | categories = replace model.categories }
