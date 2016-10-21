module Categories.List exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Maybe
import Api.Models exposing (Category, Product)
import Categories.Messages exposing (..)
import Categories.Models exposing (CategoryData)
import Utils exposing (isNothing, filterById, onClickNoDefault)


view : CategoryData -> Html Msg
view model =
    let
        rootCategories =
            List.filter (.parent >> isNothing) model.categories
    in
        div []
            [ h1 [] [ text "Categories" ]
            , button [ onClick AddCategory, class "btn-sm" ] [ text "New Category" ]
            , catTable model rootCategories
            ]


catTable : CategoryData -> List Category -> Html Msg
catTable model categories =
    table [ class "table-condensed" ]
        [ thead []
            [ tr []
                [ th [] [ text "Name" ]
                , th [] [ text "Categories" ]
                , th [] [ text "Products" ]
                ]
            ]
        , tbody [] (List.map (catRow model) categories)
        ]


catRow : CategoryData -> Category -> Html Msg
catRow model category =
    tr []
        [ td []
            [ a
                [ onClickNoDefault <| VisitCategory category.id
                , href <| "#categories/" ++ toString category.id
                ]
                [ text category.name ]
            ]
        , td [] [ text (childCount model.categories category |> toString) ]
        , td [] [ text (productCount model.products category |> toString) ]
        ]


childCount : List Category -> Category -> Int
childCount categories category =
    filterById (.parent >> Maybe.withDefault 0) category categories
        |> List.length


productCount : List Product -> Category -> Int
productCount products category =
    filterById .category category products |> List.length
