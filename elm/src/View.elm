module View exposing (..)

import Html exposing (Html, div, text)
import Html.App

import Categories.List
import Messages exposing (Msg(..))
import Models exposing (Model)


view : Model -> Html Msg
view model =
    div []
        [ page model ]


page : Model -> Html Msg
page model =
    Html.App.map CategoriesMsg (Categories.List.view model.categories)
