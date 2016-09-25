module Products.Update exposing (..)

import Dict
import Navigation
import String
import Api.Models exposing (ProductId, initialProduct)
import Products.Commands exposing (createOne)
import Products.Messages exposing (Msg(..))
import Products.Models exposing (ProductData)
import Utils exposing (replaceAllById)


update : Msg -> ProductData -> ( ProductData, Cmd Msg )
update msg model =
    case msg of
        FetchAllDone newModel ->
            ( updateModel model newModel, Cmd.none )

        FetchAllFail _ ->
            ( model, Cmd.none )

        FetchOneDone newModel ->
            ( updateModel model newModel, Cmd.none )

        FetchOneFail _ ->
            ( model, Cmd.none )

        CreateOneDone newProduct ->
            ( { model | products = newProduct :: model.products }
            , Navigation.newUrl <| "#products/" ++ toString newProduct.id
            )

        CreateOneFail _ ->
            ( model, Cmd.none )

        ToggleSKUs productId ->
            ( { model | showSKUs = toggleSKU model.showSKUs productId }, Cmd.none )

        ToggleAllSKUs ->
            ( { model | showSKUs = toggleAllSKUs model }, Cmd.none )

        VisitProduct id ->
            ( model, Navigation.newUrl <| "#products/" ++ toString id )

        VisitCategory id ->
            ( model, Navigation.newUrl <| "#categories/" ++ toString id )

        FormNameChange newName ->
            let
                productForm =
                    model.productForm

                updatedForm =
                    { productForm | name = newName }
            in
                ( { model | productForm = updatedForm }, Cmd.none )

        FormDescriptionChange newDescription ->
            let
                productForm =
                    model.productForm

                updatedForm =
                    { productForm | description = newDescription }
            in
                ( { model | productForm = updatedForm }, Cmd.none )

        FormCategoryChange newCategory ->
            let
                productForm =
                    model.productForm

                updatedForm =
                    { productForm
                        | category = String.toInt newCategory |> Result.withDefault 0
                    }
            in
                ( { model | productForm = updatedForm }, Cmd.none )

        FormActiveChange newActiveStatus ->
            let
                productForm =
                    model.productForm

                updatedForm =
                    { productForm | isActive = newActiveStatus }
            in
                ( { model | productForm = updatedForm }, Cmd.none )

        FormOrganicChange newOrganicStatus ->
            let
                productForm =
                    model.productForm

                updatedForm =
                    { productForm | isOrganic = newOrganicStatus }
            in
                ( { model | productForm = updatedForm }, Cmd.none )

        FormHeirloomChange newHeirloomStatus ->
            let
                productForm =
                    model.productForm

                updatedForm =
                    { productForm | isHeirloom = newHeirloomStatus }
            in
                ( { model | productForm = updatedForm }, Cmd.none )

        FormSouthEastChange newSouthEastStatus ->
            let
                productForm =
                    model.productForm

                updatedForm =
                    { productForm | isSouthEast = newSouthEastStatus }
            in
                ( { model | productForm = updatedForm }, Cmd.none )

        SaveForm ->
            ( { model | productForm = initialProduct }, createOne model.productForm )

        ResetForm ->
            ( { model | productForm = initialProduct }, Cmd.none )

        CancelForm ->
            ( { model | productForm = initialProduct }
            , Navigation.newUrl "#products"
            )


updateModel : ProductData -> ProductData -> ProductData
updateModel model newData =
    let
        updateAttribute =
            replaceAllById model newData
    in
        { model
            | categories = List.sortBy .name <| updateAttribute .categories
            , products = List.sortBy .name <| updateAttribute .products
            , productVariants = List.sortBy .sku <| updateAttribute .productVariants
        }


toggleSKU : Dict.Dict ProductId Bool -> ProductId -> Dict.Dict ProductId Bool
toggleSKU skus id =
    if Dict.member id skus then
        Dict.update id (Maybe.map not) skus
    else
        Dict.insert id True skus


toggleAllSKUs : ProductData -> Dict.Dict ProductId Bool
toggleAllSKUs model =
    let
        allShown =
            Dict.values model.showSKUs
                |> List.all (\v -> v == True)
                |> (&&) (not <| Dict.isEmpty model.showSKUs)
    in
        if allShown then
            Dict.map (\_ _ -> False) model.showSKUs
        else
            List.foldl (\p d -> Dict.insert p.id True d) model.showSKUs model.products
