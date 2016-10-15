module Categories.Add exposing (..)

import Html exposing (Html, h1, div, text)
import Html.App
import Categories.Form
import Categories.Messages exposing (Msg(..))
import Categories.Models exposing (CategoryData)


view : CategoryData -> Html Msg
view model =
    div []
        [ h1 [] [ text "Add Category" ]
        , Categories.Form.view
            { form = model.categoryForm
            , categories = model.categories
            , errors = model.formErrors
            }
            |> Html.App.map FormMessage
        ]
