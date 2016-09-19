module Categories.Form exposing (..)

import Html exposing (..)
import Html.Attributes exposing (type', value, name, for, selected)
import Html.Events exposing (onInput, onClick)

import Api.Models exposing (Category)
import Categories.Messages exposing (Msg(..))
import Utils exposing (onChange)


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
                    [ type' "text", value categoryForm.name, onInput FormNameChange ] []
                ]
            , br [] []
            , label []
                [ text "Description: "
                , textarea
                    [ value categoryForm.description, onInput FormDescriptionChange ] []
                ]
            , br [] []
            , label []
                [ text "Parent: "
                , select [ onChange FormParentChange ] parentOptions
                ]
            , br [] []
            , button [ onClick SaveForm ] [ text "Save" ]
            , button [ onClick ResetForm ] [ text "Reset" ]
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
