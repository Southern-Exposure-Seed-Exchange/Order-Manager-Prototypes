module Api.Http exposing (..)

import HttpBuilder
import Json.Decode as Decode
import Task

import Api.Models exposing (CategoryId, ProductId)


type Endpoint
    = CategoriesEndpoint
    | CategoryEndpoint CategoryId
    | ProductsEndpoint
    | ProductEndpoint ProductId


endpointToUrl : Endpoint -> String
endpointToUrl endpoint =
    case endpoint of
        CategoriesEndpoint ->
            "/categories/"
        CategoryEndpoint id ->
            endpointToUrl CategoriesEndpoint ++ toString id
        ProductsEndpoint ->
            "/products/"
        ProductEndpoint id ->
            endpointToUrl ProductsEndpoint ++ toString id

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
