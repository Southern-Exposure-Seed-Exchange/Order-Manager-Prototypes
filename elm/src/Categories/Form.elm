module Categories.Form exposing (..)

import Html exposing (..)
import Html.Attributes exposing (type', value, name, for, selected, class)
import Html.Events exposing (onInput, onClick)
import HttpBuilder
import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode
import Navigation
import String
import Api.Decoders exposing (categoryDecoder, productDecoder)
import Api.Encoders exposing (categoryEncoder)
import Api.Http exposing (..)
import Api.Models exposing (CategoryId, Category, Product, initialCategory)
import Utils exposing (getById, onChange, replaceBy)


type Msg
    = FormNameChange String
    | FormDescriptionChange String
    | FormParentChange String
    | SaveForm
    | ResetForm
    | CancelForm
    | UpdateOneDone CategoryId Category
    | UpdateOneFail (HttpBuilder.Error String)
    | CreateOneDone Category
    | CreateOneFail (HttpBuilder.Error String)


update : Msg -> Category -> List Category -> ( Category, List Category, Cmd Msg )
update msg form categories =
    let
        setCategoryForm id categories =
            getById categories id |> Maybe.withDefault initialCategory

        changeForm newForm =
            ( newForm, categories, Cmd.none )
    in
        case msg of
            CreateOneDone newCategory ->
                ( initialCategory
                , newCategory :: categories
                , Navigation.newUrl <| "#categories/" ++ toString newCategory.id
                )

            CreateOneFail _ ->
                ( form, categories, Cmd.none )

            UpdateOneDone categoryId newCategory ->
                ( initialCategory
                , replaceBy .id newCategory categories
                , Navigation.newUrl <| "#categories/" ++ toString categoryId
                )

            UpdateOneFail _ ->
                ( form, categories, Cmd.none )

            FormNameChange newName ->
                changeForm { form | name = newName }

            FormDescriptionChange newDescription ->
                changeForm { form | description = newDescription }

            FormParentChange newParent ->
                changeForm { form | parent = String.toInt newParent |> Result.toMaybe }

            SaveForm ->
                let
                    saveCommand =
                        if form.id == 0 then
                            createOne
                        else
                            updateOne
                in
                    ( form, categories, saveCommand form )

            ResetForm ->
                changeForm <| setCategoryForm form.id categories

            CancelForm ->
                let
                    categoryUrl =
                        if form.id == 0 then
                            ""
                        else
                            toString form.id
                in
                    ( setCategoryForm form.id categories
                    , categories
                    , Navigation.newUrl <| "#categories/" ++ categoryUrl
                    )


updateOne : Category -> Cmd Msg
updateOne category =
    put (CategoryEndpoint category.id)
        (categoriesEncoder category)
        ("category" := categoryDecoder)
        UpdateOneFail
        (UpdateOneDone category.id)


createOne : Category -> Cmd Msg
createOne category =
    post CategoriesEndpoint
        (categoriesEncoder category)
        ("category" := categoryDecoder)
        CreateOneFail
        CreateOneDone


categoriesEncoder : Category -> Encode.Value
categoriesEncoder category =
    Encode.object
        [ ( "category", categoryEncoder category ) ]


view : Category -> List Category -> Html Msg
view categoryForm categories =
    let
        otherCategories =
            List.filter (\c -> c.id /= categoryForm.id) categories

        parentOptions =
            option [] [ text "None" ] :: List.map (parentOption categoryForm) otherCategories
    in
        div []
            [ label []
                [ text "Name: "
                , input
                    [ type' "text", value categoryForm.name, onInput FormNameChange ]
                    []
                ]
            , br [] []
            , label []
                [ text "Description: "
                , textarea
                    [ value categoryForm.description, onInput FormDescriptionChange ]
                    []
                ]
            , br [] []
            , label []
                [ text "Parent: "
                , select [ onChange FormParentChange ] parentOptions
                ]
            , br [] []
            , button [ onClick SaveForm ] [ text "Save" ]
            , text " "
            , button [ onClick ResetForm ] [ text "Reset" ]
            , text " "
            , button [ onClick CancelForm ] [ text "Cancel" ]
            ]


parentOption : Category -> Category -> Html msg
parentOption category optionCategory =
    let
        isParent =
            Maybe.map (\p -> p == optionCategory.id) category.parent
                |> Maybe.withDefault False

        attributes =
            if isParent then
                [ selected True ]
            else
                []
    in
        option ([ value <| toString optionCategory.id ] ++ attributes)
            [ text optionCategory.name ]
