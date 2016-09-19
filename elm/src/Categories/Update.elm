module Categories.Update exposing (..)

import Navigation
import String

import Api.Models exposing (CategoryId, initialCategory)
import Categories.Commands exposing (createOne, updateOne)
import Categories.Messages exposing (Msg(..))
import Categories.Models exposing (CategoryData)
import Utils exposing (replaceBy, replaceAllById, getById)


update : Msg -> CategoryData -> ( CategoryData, Cmd Msg )
update msg model =
    case msg of
        FetchAllDone newModel ->
            ( updateModel model newModel, Cmd.none )
        FetchAllFail _ ->
            ( model, Cmd.none )
        FetchOneDone categoryId newModel ->
            ( setCategoryForm categoryId <| updateModel model newModel
            , Cmd.none )
        FetchOneFail _ ->
            ( model, Cmd.none )
        UpdateOneDone categoryId newCategory ->
            ( { model | categories = replaceBy .id newCategory model.categories }
            , Navigation.newUrl <| "#categories/" ++ toString categoryId
            )
        UpdateOneFail _ ->
            ( model, Cmd.none )
        CreateOneDone newCategory ->
            ( { model | categories = newCategory :: model.categories }
            , Navigation.newUrl <| "#categories/" ++ toString newCategory.id
            )
        CreateOneFail _ ->
            ( model, Cmd.none )
        VisitCategory id ->
            ( model, Navigation.newUrl <| "#categories/" ++ toString id )
        EditCategory id ->
            ( setCategoryForm id model
            , Navigation.newUrl <| "#categories/" ++ toString id ++ "/edit")
        VisitProduct id ->
            ( model, Navigation.newUrl <| "#products/" ++ toString id )
        FormNameChange newName ->
            let
                categoryForm =
                    model.categoryForm
                updatedForm =
                    { categoryForm | name = newName }
            in
               ( { model | categoryForm = updatedForm }, Cmd.none )
        FormDescriptionChange newDescription ->
            let
                categoryForm =
                    model.categoryForm
                updatedForm =
                    { categoryForm | description = newDescription }
            in
               ( { model | categoryForm = updatedForm }, Cmd.none )
        FormParentChange newParent ->
            let
                categoryForm =
                    model.categoryForm
                updatedForm =
                    { categoryForm | parent = String.toInt newParent |> Result.toMaybe }
            in
               ( { model | categoryForm = updatedForm }, Cmd.none )
        SaveForm ->
            let
                saveCommand =
                    if model.categoryForm.id == 0 then
                       createOne
                    else
                        updateOne
            in
                ( model, saveCommand model.categoryForm )
        ResetForm ->
            ( setCategoryForm model.categoryForm.id model, Cmd.none )
        CancelForm ->
            let
                categoryUrl =
                    if model.categoryForm.id == 0 then
                       ""
                    else
                        toString model.categoryForm.id
            in
                ( setCategoryForm model.categoryForm.id model
                , Navigation.newUrl <| "#categories/" ++ categoryUrl )


setCategoryForm : CategoryId -> CategoryData -> CategoryData
setCategoryForm id model =
    let
        categoryForm =
            getById model.categories id
                |> Maybe.withDefault initialCategory
    in
       { model | categoryForm = categoryForm }


updateModel : CategoryData -> CategoryData -> CategoryData
updateModel model newData =
    let
        updateAttribute =
            replaceAllById model newData
    in
        { model
            | categories = List.sortBy .name <| updateAttribute .categories
            , products = List.sortBy .name <| updateAttribute .products
            }
