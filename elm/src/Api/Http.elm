module Api.Http exposing (..)

import HttpBuilder
import Json.Decode as Decode
import Task

import Api.Models exposing (CategoryId)


type Endpoint
    = CategoriesEndpoint
    | CategoryEndpoint CategoryId
    | ProductsEndpoint


endpointToUrl : Endpoint -> String
endpointToUrl endpoint =
    case endpoint of
        CategoriesEndpoint ->
            "/categories/"
        CategoryEndpoint id ->
            endpointToUrl CategoriesEndpoint ++ toString id
        ProductsEndpoint ->
            "/products/"

get : Endpoint -> Decode.Decoder a -> (HttpBuilder.Error String -> msg) -> (a -> msg) -> Cmd msg
get endpoint decoder failMsg successMsg =
    endpointToUrl endpoint
        |> HttpBuilder.get
        |> HttpBuilder.withHeaders
            [ ("Accept","application/json")
            , ("Content-Type", "application/json")
            ]
        |> HttpBuilder.send (HttpBuilder.jsonReader decoder) (HttpBuilder.stringReader)
        |> Task.perform failMsg (\r -> successMsg r.data)
