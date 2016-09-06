module View exposing (..)

import Html exposing (Html, div, text)
import Html.App

import Categories.List
import Messages exposing (Msg(..))
import Models exposing (Model)
import Routing exposing (Route(..))


view : Model -> Html Msg
view model =
    div []
        [ page model ]


page : Model -> Html Msg
page model =
    case model.route of
        CategoriesRoute ->
            Categories.List.view { categories = model.categories, products = model.products }
                |> Html.App.map CategoriesMsg
        NotFoundRoute ->
            notFound model


notFound : Model -> Html Msg
notFound model = div [] [text "404 - Page Not Found"]
