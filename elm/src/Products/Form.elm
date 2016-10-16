module Products.Form exposing (..)

import Html exposing (..)
import Html.Attributes exposing (type', class, value, selected, checked)
import Html.Events exposing (onCheck, onClick, onInput)
import HttpBuilder exposing (Error(..))
import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode
import Navigation
import String
import Api.Decoders exposing (productDecoder)
import Api.Encoders exposing (productEncoder)
import Api.Errors exposing (parseErrors)
import Api.Http exposing (..)
import Api.Models exposing (Category, Product, ProductId, initialProduct)
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
    | UpdateOneDone ProductId Product
    | SaveFail (HttpBuilder.Error String)


type alias Model =
    { form : Product
    , errors : FormErrors
    , products : List Product
    , categories : List Category
    }


type alias FormErrors =
    { name : String
    , category : String
    }


initialErrors : FormErrors
initialErrors =
    { name = ""
    , category = ""
    }


getErrors : String -> FormErrors
getErrors data =
    let
        assignErrors { source, detail } errors =
            case source of
                "name" ->
                    { errors | name = detail }

                "category" ->
                    { errors | category = detail }

                _ ->
                    errors
    in
        parseErrors assignErrors initialErrors data


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ form } as model) =
    let
        changeForm newForm =
            ( { model | form = newForm }, Cmd.none )

        setForm id products =
            getById products id |> Maybe.withDefault initialProduct
    in
        case msg of
            CreateOneDone newProduct ->
                ( { model | products = newProduct :: model.products }
                , Navigation.newUrl <| "#products/" ++ toString newProduct.id
                )

            UpdateOneDone productId newProduct ->
                ( { model
                    | form = initialProduct
                    , products = replaceBy .id newProduct model.products
                  }
                , Navigation.newUrl <| "#products/" ++ toString productId
                )

            SaveFail error ->
                case error of
                    BadResponse resp ->
                        ( { model | errors = getErrors resp.data }
                        , Cmd.none
                        )

                    _ ->
                        ( model, Cmd.none )

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
                    ( model, saveCommand form )

            ResetForm ->
                ( { model | form = setForm form.id model.products, errors = initialErrors }
                , Cmd.none
                )

            CancelForm ->
                let
                    url =
                        if form.id == 0 then
                            ""
                        else
                            toString form.id
                in
                    ( { model
                        | form = setForm form.id model.products
                        , errors = initialErrors
                      }
                    , Navigation.newUrl <| "#products/" ++ url
                    )


createOne : Product -> Cmd Msg
createOne product =
    post ProductsEndpoint
        (productsEncoder product)
        ("product" := productDecoder)
        SaveFail
        CreateOneDone


updateOne : Product -> Cmd Msg
updateOne product =
    put (ProductEndpoint product.id)
        (productsEncoder product)
        ("product" := productDecoder)
        SaveFail
        (UpdateOneDone product.id)


productsEncoder : Product -> Encode.Value
productsEncoder product =
    Encode.object [ ( "product", productEncoder product ) ]


showError : String -> Html msg
showError error =
    if String.isEmpty error then
        text ""
    else
        p [ class "form-error" ] [ text error ]


view : Model -> Html Msg
view ({ form, categories } as model) =
    div []
        [ label []
            [ text "Name: "
            , input
                [ type' "text"
                , value form.name
                , onInput NameChange
                ]
                []
            , showError model.errors.name
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
                [ option
                    (if form.id == 0 then
                        [ selected True ]
                     else
                        []
                    )
                    [ text "Select a Category" ]
                ]
                    ++ List.map (categoryOption form) categories
            , showError model.errors.category
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
