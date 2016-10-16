module Products.Update exposing (..)

import Dict
import Navigation
import Api.Models exposing (ProductId, initialProduct)
import Products.Commands exposing (deleteOne)
import Products.Form exposing (initialErrors)
import Products.Messages exposing (Msg(..))
import Products.Models exposing (ProductData)
import Utils exposing (getById, replaceAllById)


update : Msg -> ProductData -> ( ProductData, Cmd Msg )
update msg model =
    case msg of
        FetchAllDone newModel ->
            ( { model
                | products = newModel.products
                , productVariants = newModel.productVariants
                , categories = replaceAllById model newModel .categories
              }
            , Cmd.none
            )

        FetchAllFail _ ->
            ( model, Cmd.none )

        FetchOneDone productId newModel ->
            ( setProductForm productId <| updateModel model newModel
            , Cmd.none
            )

        FetchOneFail _ ->
            ( model, Cmd.none )

        DeleteOneDone id ->
            let
                updatedProducts =
                    List.filter (\p -> p.id /= id) model.products
            in
                ( { model | products = updatedProducts }
                , Navigation.newUrl "#products"
                )

        DeleteOneFail _ ->
            ( model, Cmd.none )

        ToggleSKUs productId ->
            ( { model | showSKUs = toggleSKU model.showSKUs productId }, Cmd.none )

        ToggleAllSKUs ->
            ( { model | showSKUs = toggleAllSKUs model }, Cmd.none )

        VisitProduct id ->
            ( model, Navigation.newUrl <| "#products/" ++ toString id )

        VisitCategory id ->
            ( model, Navigation.newUrl <| "#categories/" ++ toString id )

        AddProduct ->
            ( model, Navigation.newUrl <| "#products/add" )

        EditProduct id ->
            ( setProductForm id model
            , Navigation.newUrl <| "#products/" ++ toString id ++ "/edit"
            )

        DeleteProduct id ->
            ( model, deleteOne id )

        FormMessage subMsg ->
            let
                ( updatedForm, cmd ) =
                    Products.Form.update subMsg
                        { form = model.productForm
                        , errors = model.formErrors
                        , products = model.products
                        , categories = model.categories
                        }
            in
                ( { model
                    | productForm = updatedForm.form
                    , formErrors = updatedForm.errors
                    , products = updatedForm.products
                    , categories = updatedForm.categories
                  }
                , Cmd.map FormMessage cmd
                )


setProductForm : ProductId -> ProductData -> ProductData
setProductForm id model =
    let
        productForm =
            getById model.products id
                |> Maybe.withDefault initialProduct
    in
        { model
            | productForm = productForm
            , formErrors = initialErrors
        }


updateModel : ProductData -> ProductData -> ProductData
updateModel model newData =
    let
        updateAttribute =
            replaceAllById model newData
    in
        { model
            | categories = updateAttribute .categories
            , products = updateAttribute .products
            , productVariants = updateAttribute .productVariants
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
                |> (&&) (Dict.size model.showSKUs == List.length model.products)
    in
        if allShown then
            Dict.map (\_ _ -> False) model.showSKUs
        else
            List.foldl (\p d -> Dict.insert p.id True d) model.showSKUs model.products
