module Main exposing (..)

import Html.App
import Navigation

import Categories.Commands exposing (fetchAll)

import Models exposing (Model, initialModel)
import Messages exposing (Msg(..))
import Update exposing (update)
import View exposing (view)
import Routing exposing (Route)


init : Result String Route -> ( Model, Cmd Msg )
init result =
    let currentRoute =
        Routing.routeFromResult result
    in
       ( initialModel currentRoute, Cmd.map CategoriesMsg fetchAll )


urlUpdate : Result String Route -> Model -> ( Model, Cmd Msg )
urlUpdate result model =
    let
        currentRoute =
            Routing.routeFromResult result
    in
       ( { model | route = currentRoute }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never
main =
    Navigation.program Routing.parser
        { init = init
        , view = view
        , update = update
        , urlUpdate = urlUpdate
        , subscriptions = subscriptions
        }
