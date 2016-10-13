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
            "/api/categories/"

        CategoryEndpoint id ->
            endpointToUrl CategoriesEndpoint ++ toString id

        ProductsEndpoint ->
            "/api/products/"

        ProductEndpoint id ->
            endpointToUrl ProductsEndpoint ++ toString id


defaultHeaders : HttpBuilder.RequestBuilder -> HttpBuilder.RequestBuilder
defaultHeaders =
    HttpBuilder.withHeaders
        [ ( "Accept", "application/json" ), ( "Content-Type", "application/json" ) ]


get : Endpoint -> Decode.Decoder a -> (HttpBuilder.Error String -> msg) -> (a -> msg) -> Cmd msg
get endpoint decoder failMsg successMsg =
    endpointToUrl endpoint
        |> HttpBuilder.get
        |> defaultHeaders
        |> HttpBuilder.send (HttpBuilder.jsonReader decoder) (HttpBuilder.stringReader)
        |> Task.perform failMsg (\r -> successMsg r.data)


put : Endpoint -> Decode.Value -> Decode.Decoder a -> (HttpBuilder.Error String -> msg) -> (a -> msg) -> Cmd msg
put endpoint params decoder failMsg successMsg =
    endpointToUrl endpoint
        |> HttpBuilder.put
        |> defaultHeaders
        |> HttpBuilder.withJsonBody params
        |> HttpBuilder.send (HttpBuilder.jsonReader decoder) (HttpBuilder.stringReader)
        |> Task.perform failMsg (\r -> successMsg r.data)


post : Endpoint -> Decode.Value -> Decode.Decoder a -> (HttpBuilder.Error String -> msg) -> (a -> msg) -> Cmd msg
post endpoint params decoder failMsg successMsg =
    endpointToUrl endpoint
        |> HttpBuilder.post
        |> defaultHeaders
        |> HttpBuilder.withJsonBody params
        |> HttpBuilder.send (HttpBuilder.jsonReader decoder) (HttpBuilder.stringReader)
        |> Task.perform failMsg (\r -> successMsg r.data)


delete : Endpoint -> Decode.Decoder a -> (HttpBuilder.Error String -> msg) -> (a -> msg) -> Cmd msg
delete endpoint decoder failMsg successMsg =
    endpointToUrl endpoint
        |> HttpBuilder.delete
        |> defaultHeaders
        |> HttpBuilder.send (HttpBuilder.jsonReader decoder) (HttpBuilder.stringReader)
        |> Task.perform failMsg (\r -> successMsg r.data)
