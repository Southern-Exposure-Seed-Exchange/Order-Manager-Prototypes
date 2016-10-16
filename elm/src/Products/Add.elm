module Products.Add exposing (..)

import Html exposing (..)
import Html.App
import Products.Form
import Products.Models exposing (ProductData)
import Products.Messages exposing (Msg(..))


view : ProductData -> Html Msg
view model =
    div []
        [ h1 [] [ text "Add Product" ]
        , Products.Form.view
            { form = model.productForm
            , products = model.products
            , errors = model.formErrors
            , categories = model.categories
            }
            |> Html.App.map FormMessage
        ]
