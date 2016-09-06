module View exposing (..)

import Html exposing (..)
import Html.App
import Html.Events exposing (onClick)

import Categories.List
import Messages exposing (Msg(..))
import Models exposing (Model)
import Routing exposing (Route(..))


view : Model -> Html Msg
view model =
    div []
        [ nav
        , page model ]


nav : Html Msg
nav =
    div []
        [ span [] [text "Menu:  "]
        , a [ onClick (RoutingMsg DashboardRoute) ]
            [ text "Dashboard " ]
        , a [ onClick (RoutingMsg CategoriesRoute) ]
            [ text "Categories " ]
        ]


page : Model -> Html Msg
page model =
    case model.route of
        DashboardRoute ->
            dashboard
        CategoriesRoute ->
            Categories.List.view { categories = model.categories, products = model.products }
                |> Html.App.map CategoriesMsg
        NotFoundRoute ->
            notFound model


notFound : Model -> Html Msg
notFound model = div [] [text "404 - Page Not Found"]

dashboard : Html Msg
dashboard =
    div []
        [ h1 [] [ text "Dashboard" ]
        , p [] [ text "At some point there'll be cool stuff here." ]
        , p [] [ text "Backends should implement:" ]
        , ul []
            [ li [] [ text "Categories - list" ]
            ]
        ]
