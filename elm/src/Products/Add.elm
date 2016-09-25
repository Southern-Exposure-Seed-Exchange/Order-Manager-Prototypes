module Products.Add exposing (..)

import Html exposing (..)
import Html.Attributes exposing (type', class, value, selected, checked)
import Html.Events exposing (onCheck, onClick, onInput)
import Api.Models exposing (Category, Product)
import Products.Models exposing (ProductData)
import Products.Messages exposing (Msg(..))
import Utils exposing (onChange)


view : ProductData -> Html Msg
view model =
    div []
        [ h1 [] [ text "Add Product" ]
        , label []
            [ text "Name: "
            , input
                [ type' "text"
                , value model.productForm.name
                , onInput FormNameChange
                ]
                []
            ]
        , label []
            [ text "Description:"
            , textarea
                [ value model.productForm.description
                , onInput FormDescriptionChange
                ]
                []
            ]
        , label []
            [ text "Category:"
            , select [ onChange FormCategoryChange ] <|
                List.map (categoryOption model.productForm) model.categories
            ]
        , label []
            [ text "Is Active:"
            , input
                [ type' "checkbox"
                , onCheck FormActiveChange
                , checked model.productForm.isActive
                ]
                []
            ]
        , label []
            [ text "Is Organic:"
            , input
                [ type' "checkbox"
                , onCheck FormOrganicChange
                , checked model.productForm.isOrganic
                ]
                []
            ]
        , label []
            [ text "Is Heirloom:"
            , input
                [ type' "checkbox"
                , onCheck FormHeirloomChange
                , checked model.productForm.isHeirloom
                ]
                []
            ]
        , label []
            [ text "Is South East:"
            , input
                [ type' "checkbox"
                , onCheck FormSouthEastChange
                , checked model.productForm.isSouthEast
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
