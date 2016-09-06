module Main exposing (..)

import Html exposing (Html, div, text)
import Html.App

import Categories.Commands exposing (fetchAll)

import Models exposing (Model, initialModel)
import Messages exposing (Msg(..))
import Update exposing (update)
import View exposing (view)


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.map CategoriesMsg fetchAll )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never
main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
