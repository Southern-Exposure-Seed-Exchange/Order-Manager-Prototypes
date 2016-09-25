module Products.Form exposing (..)

import Html exposing (..)
import Html.Attributes exposing (type', class, value, selected, checked)
import Html.Events exposing (onCheck, onClick, onInput)
import HttpBuilder
import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode
import Navigation
import String
import Api.Decoders exposing (productDecoder)
import Api.Encoders exposing (productEncoder)
import Api.Http exposing (..)
import Api.Models exposing (Category, Product, ProductId, initialProduct)
import Products.Models exposing (ProductData)
import Utils exposing (getById, onChange, replaceBy)


type Msg
    = NameChange String
    | DescriptionChange String
    | CategoryChange String
    | ActiveChange Bool
    | OrganicChange Bool
    | HeirloomChange Bool
    | SouthEastChange Bool
    | SaveForm
    | ResetForm
    | CancelForm
    | CreateOneDone Product
    | CreateOneFail (HttpBuilder.Error String)
    | UpdateOneDone ProductId Product
    | UpdateOneFail (HttpBuilder.Error String)


update : Msg -> Product -> ProductData -> ( Product, List Product, Cmd Msg )
update msg form model =
    let
        changeForm newForm =
            ( newForm, model.products, Cmd.none )

        setForm id products =
            getById products id |> Maybe.withDefault initialProduct
    in
        case msg of
            CreateOneDone newProduct ->
                ( form
                , newProduct :: model.products
                , Navigation.newUrl <| "#products/" ++ toString newProduct.id
                )

            CreateOneFail _ ->
                ( form, model.products, Cmd.none )

            UpdateOneDone productId newProduct ->
                ( initialProduct
                , replaceBy .id newProduct model.products
                , Navigation.newUrl <| "#products/" ++ toString productId
                )

            UpdateOneFail _ ->
                ( form, model.products, Cmd.none )

            NameChange newName ->
                changeForm { form | name = newName }

            DescriptionChange newDescription ->
                changeForm { form | description = newDescription }

            CategoryChange newCategory ->
                changeForm { form | category = String.toInt newCategory |> Result.withDefault 0 }

            ActiveChange newActiveStatus ->
                changeForm { form | isActive = newActiveStatus }

            OrganicChange newOrganicStatus ->
                changeForm { form | isOrganic = newOrganicStatus }

            HeirloomChange newHeirloomStatus ->
                changeForm { form | isHeirloom = newHeirloomStatus }

            SouthEastChange newSouthEastStatus ->
                changeForm { form | isSouthEast = newSouthEastStatus }

            SaveForm ->
                let
                    saveCommand =
                        if form.id == 0 then
                            createOne
                        else
                            updateOne
                in
                    ( form, model.products, saveCommand form )

            ResetForm ->
                changeForm <| setForm form.id model.products

            CancelForm ->
                let
                    url =
                        if form.id == 0 then
                            ""
                        else
                            toString form.id
                in
                    ( setForm form.id model.products
                    , model.products
                    , Navigation.newUrl <| "#products/" ++ url
                    )


createOne : Product -> Cmd Msg
createOne product =
    post ProductsEndpoint
        (productsEncoder product)
        ("product" := productDecoder)
        CreateOneFail
        CreateOneDone


updateOne : Product -> Cmd Msg
updateOne product =
    put (ProductEndpoint product.id)
        (productsEncoder product)
        ("product" := productDecoder)
        UpdateOneFail
        (UpdateOneDone product.id)


productsEncoder : Product -> Encode.Value
productsEncoder product =
    Encode.object [ ( "product", productEncoder product ) ]


view : Product -> List Category -> Html Msg
view form categories =
    div []
        [ label []
            [ text "Name: "
            , input
                [ type' "text"
                , value form.name
                , onInput NameChange
                ]
                []
            ]
        , label []
            [ text "Description:"
            , textarea
                [ value form.description
                , onInput DescriptionChange
                ]
                []
            ]
        , label []
            [ text "Category:"
            , select [ onChange CategoryChange ] <|
                List.map (categoryOption form) categories
            ]
        , label []
            [ text "Is Active:"
            , input
                [ type' "checkbox"
                , onCheck ActiveChange
                , checked form.isActive
                ]
                []
            ]
        , label []
            [ text "Is Organic:"
            , input
                [ type' "checkbox"
                , onCheck OrganicChange
                , checked form.isOrganic
                ]
                []
            ]
        , label []
            [ text "Is Heirloom:"
            , input
                [ type' "checkbox"
                , onCheck HeirloomChange
                , checked form.isHeirloom
                ]
                []
            ]
        , label []
            [ text "Is South East:"
            , input
                [ type' "checkbox"
                , onCheck SouthEastChange
                , checked form.isSouthEast
                ]
                []
            ]
        , button [ onClick SaveForm ] [ text "Save" ]
        , text " "
        , button [ onClick ResetForm ] [ text "Reset" ]
        , text " "
        , button [ onClick CancelForm ] [ text "Cancel" ]
        ]


categoryOption : Product -> Category -> Html msg
categoryOption product category =
    let
        attributes =
            if product.category == category.id then
                [ selected True ]
            else
                []
    in
        option ([ value <| toString category.id ] ++ attributes)
            [ text category.name ]
