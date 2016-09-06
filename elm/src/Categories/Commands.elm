module Categories.Commands exposing (..)

import Http
import HttpBuilder
import Json.Decode as Decode exposing ((:=))
import Task

import Categories.Models exposing (CategoryId, Category, CategoryData, Product)
import Categories.Messages exposing (..)

fetchAll : Cmd Msg
fetchAll =
    HttpBuilder.get fetchAllUrl
        |> HttpBuilder.withHeaders 
            [ ("Accept","application/json")
            , ("Content-Type", "application/json")
            ]
        |> HttpBuilder.send (HttpBuilder.jsonReader categoriesDecoder) (HttpBuilder.stringReader)
        |> Task.perform FetchAllFail (\r -> FetchAllDone r.data)

fetchAllUrl : String
fetchAllUrl =
    "/categories"

categoriesDecoder : Decode.Decoder CategoryData
categoriesDecoder =
    Decode.object2 CategoryData 
        ("category" := Decode.list categoryDecoder)
        ("product" := Decode.list productDecoder)

categoryDecoder : Decode.Decoder Category
categoryDecoder =
    Decode.object4 Category
        ("id" := Decode.int)
        ("name" := Decode.string)
        ("description" := Decode.string)
        (Decode.maybe("parent" := Decode.int))


productDecoder : Decode.Decoder Product
productDecoder =
    Decode.object8 Product
        ("id" := Decode.int)
        ("name" := Decode.string)
        ("description" := Decode.string)
        ("category" := Decode.int)
        ("isActive" := Decode.bool)
        ("isOrganic" := Decode.bool)
        ("isHeirloom" := Decode.bool)
        ("isSouthEast" := Decode.bool)
