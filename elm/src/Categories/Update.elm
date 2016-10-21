module Categories.Update exposing (..)

import Navigation
import Api.Models exposing (CategoryId, initialCategory)
import Categories.Commands exposing (deleteOne, confirmCategoryDeletion)
import Categories.Form
import Categories.Messages exposing (Msg(..))
import Categories.Models exposing (CategoryData)
import Utils exposing (getById, replaceAllById)


update : Msg -> CategoryData -> ( CategoryData, Cmd Msg )
update msg model =
    case msg of
        FetchAllDone newModel ->
            ( { model
                | products = newModel.products
                , categories = newModel.categories
              }
            , Cmd.none
            )

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
            ( model
            , confirmCategoryDeletion
                ( id
                , "Are you sure you want to delete this Category? This is irreversible!"
                )
            )

        CategoryDeletionConfirmed id ->
            ( model, deleteOne id )

        VisitProduct id ->
            ( model, Navigation.newUrl <| "#products/" ++ toString id )

        FormMessage subMsg ->
            let
                ( updatedForm, cmd ) =
                    Categories.Form.update subMsg
                        { form = model.categoryForm
                        , categories = model.categories
                        , errors = model.formErrors
                        }
            in
                ( { model
                    | categoryForm = updatedForm.form
                    , categories =
                        updatedForm.categories
                    , formErrors = updatedForm.errors
                  }
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
            | categories = updateAttribute .categories
            , products = updateAttribute .products
        }
