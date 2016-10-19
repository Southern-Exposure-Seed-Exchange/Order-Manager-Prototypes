module Main exposing (..)

import Navigation
import Commands exposing (fetchForRoute, setPageTitle)
import Messages exposing (Msg)
import Models exposing (Model, initialModel, UIState, initialUIState)
import Routing exposing (Route(..))
import Update exposing (update, updateUI)
import View exposing (view)


init : Result String Route -> ( Model, Cmd Msg )
init result =
    let
        currentRoute =
            Routing.routeFromResult result
    in
        ( initialModel currentRoute
        , Cmd.batch
            [ setPageTitle currentRoute
            , fetchForRoute currentRoute
            ]
        )


urlUpdate : Result String Route -> Model -> ( Model, Cmd Msg )
urlUpdate result model =
    let
        updatedModel =
            { model | route = Routing.routeFromResult result }
    in
        ( { updatedModel
            | uiState = updateUI updatedModel.route model.uiState
          }
        , Cmd.batch
            [ fetchForRoute updatedModel.route
            , setPageTitle updatedModel.route
            ]
        )


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
