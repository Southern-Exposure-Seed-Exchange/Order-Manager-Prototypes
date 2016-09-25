module Categories.Update exposing (..)

import Navigation
import String
import Api.Models exposing (CategoryId, initialCategory)
import Categories.Commands exposing (deleteOne)
import Categories.Form
import Categories.Messages exposing (Msg(..))
import Categories.Models exposing (CategoryData)
import Utils exposing (replaceAllById, getById)


update : Msg -> CategoryData -> ( CategoryData, Cmd Msg )
update msg model =
    case msg of
        FetchAllDone newModel ->
            ( updateModel model newModel, Cmd.none )

        FetchAllFail _ ->
            ( model, Cmd.none )

        FetchOneDone categoryId newModel ->
            ( setCategoryForm categoryId <| updateModel model newModel
            , Cmd.none
            )

        FetchOneFail _ ->
            ( model, Cmd.none )

        DeleteOneDone id ->
            ( { model | categories = List.filter (\c -> c.id /= id) model.categories }
            , Navigation.newUrl "#categories"
            )

        DeleteOneFail _ ->
            ( model, Cmd.none )

        AddCategory ->
            ( model, Navigation.newUrl "#categories/add" )

        VisitCategory id ->
            ( model, Navigation.newUrl <| "#categories/" ++ toString id )

        EditCategory id ->
            ( setCategoryForm id model
            , Navigation.newUrl <| "#categories/" ++ toString id ++ "/edit"
            )

        DeleteCategory id ->
            ( model, deleteOne id )

        VisitProduct id ->
            ( model, Navigation.newUrl <| "#products/" ++ toString id )

        FormMessage subMsg ->
            let
                ( updatedForm, updatedCategories, cmd ) =
                    Categories.Form.update subMsg model.categoryForm model.categories
            in
                ( { model | categoryForm = updatedForm, categories = updatedCategories }
                , Cmd.map FormMessage cmd
                )


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
