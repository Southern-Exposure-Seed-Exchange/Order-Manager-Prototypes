module Categories.Commands exposing (..)

import Http
import HttpBuilder
import Json.Decode as Decode exposing ((:=))
import Task

import Categories.Models exposing (CategoryId, Category, CategoryData, Product)
import Categories.Messages exposing (..)

fetchAll : Cmd Msg
fetchAll = get fetchAllUrl categoriesDecoder FetchAllFail FetchAllDone

fetchAllUrl : String
fetchAllUrl =
    "/categories"

fetchOne : CategoryId -> Cmd Msg
fetchOne categoryId =
    get (fetchOneUrl categoryId) categoryDecoder FetchOneFail FetchOneDone

fetchOneUrl categoryId =
    "/categories/" ++ toString categoryId

get : String -> Decode.Decoder a -> (HttpBuilder.Error String -> Msg) -> (a -> Msg) -> Cmd Msg
get url decoder failMsg successMsg =
    HttpBuilder.get url
        |> HttpBuilder.withHeaders
            [ ("Accept","application/json")
            , ("Content-Type", "application/json")
            ]
        |> HttpBuilder.send (HttpBuilder.jsonReader decoder) (HttpBuilder.stringReader)
        |> Task.perform failMsg (\r -> successMsg r.data)


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
