module Categories.List exposing (..)

import Html exposing (..)
import Maybe

import Categories.Messages exposing (..)
import Categories.Models exposing (Category)


view : List Category -> Html Msg
view categories =
    div []
        [ h1 [] [ text "Categories" ]
        , catTable categories ]


catTable : List Category -> Html Msg
catTable categories =
    let
        isNothing maybe =
            case maybe of
                Nothing ->
                    True
                Just _ ->
                    False
        rootCategories =
            List.filter (\cat -> isNothing cat.parent) categories
    in
        table []
            [ thead []
                [ tr []
                    [ th [] [ text "Name" ]
                    , th [] [ text "Categories" ]
                    , th [] [ text "Products" ]
                    ]
                ]
            , tbody [] (List.map (catRow categories) rootCategories)
            ]


catRow : List Category -> Category -> Html Msg
catRow categories category =
    tr []
        [ td [] [ text category.name ]
        , td [] [ text (childCount categories category |> toString) ]
        , td [] [ text "0" ]
        ]


childCount : List Category -> Category -> Int
childCount categories category =
    List.filter (\c -> Maybe.withDefault 0 c.parent == category.id) categories
        |> List.length