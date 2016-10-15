module Categories.Form exposing (..)

import Html exposing (..)
import Html.Attributes exposing (type', value, name, for, selected, class)
import Html.Events exposing (onInput, onClick)
import HttpBuilder
import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode
import Navigation
import String
import Validate exposing (ifBlank)
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


type alias FormErrors =
    { name : String
    }


initialErrors : FormErrors
initialErrors =
    { name = ""
    }


type alias Model =
    { categories : List Category
    , form : Category
    , errors : FormErrors
    }


validateCategory : Category -> FormErrors
validateCategory =
    let
        convert ( fieldName, error ) errors =
            case fieldName of
                "name" ->
                    { errors | name = error }

                _ ->
                    errors
    in
        Validate.all [ .name >> ifBlank ( "name", "A name is required." ) ]
            >> List.foldl convert initialErrors
            >> Debug.log "validate"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ categories, form } as model) =
    let
        setCategoryForm id categories =
            getById categories id |> Maybe.withDefault initialCategory

        changeForm newForm =
            ( { model
                | form = newForm
                , errors = validateCategory newForm
              }
            , Cmd.none
            )
    in
        case msg of
            CreateOneDone newCategory ->
                ( { model
                    | form = initialCategory
                    , categories = newCategory :: categories
                    , errors = initialErrors
                  }
                , Navigation.newUrl <| "#categories/" ++ toString newCategory.id
                )

            CreateOneFail _ ->
                ( model, Cmd.none )

            UpdateOneDone categoryId newCategory ->
                ( { model
                    | form = initialCategory
                    , categories = replaceBy .id newCategory categories
                    , errors = initialErrors
                  }
                , Navigation.newUrl <| "#categories/" ++ toString categoryId
                )

            UpdateOneFail _ ->
                ( model, Cmd.none )

            FormNameChange newName ->
                changeForm { form | name = newName }

            FormDescriptionChange newDescription ->
                changeForm { form | description = newDescription }

            FormParentChange newParent ->
                changeForm { form | parent = String.toInt newParent |> Result.toMaybe }

            SaveForm ->
                let
                    validation =
                        validateCategory model.form

                    isValid =
                        validation == initialErrors

                    saveCommand =
                        if form.id == 0 then
                            createOne
                        else
                            updateOne
                in
                    if isValid then
                        ( model, saveCommand form )
                    else
                        ( { model | errors = validation }, Cmd.none )

            ResetForm ->
                ( { model
                    | form = setCategoryForm form.id categories
                    , errors = initialErrors
                  }
                , Cmd.none
                )

            CancelForm ->
                let
                    categoryUrl =
                        if form.id == 0 then
                            ""
                        else
                            toString form.id
                in
                    ( { model | form = setCategoryForm form.id categories }
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


view : Model -> Html Msg
view { form, categories, errors } =
    let
        otherCategories =
            List.filter (\c -> c.id /= form.id) categories

        parentOptions =
            option [] [ text "None" ] :: List.map (parentOption form) otherCategories
    in
        div []
            [ label []
                [ text "Name: "
                , input
                    [ type' "text", value form.name, onInput FormNameChange ]
                    []
                , if String.isEmpty errors.name then
                    text ""
                  else
                    p [ class "form-error" ] [ text errors.name ]
                ]
            , br [] []
            , label []
                [ text "Description: "
                , textarea
                    [ value form.description, onInput FormDescriptionChange ]
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
