module Categories.Commands exposing (..)

import Json.Decode as Decode exposing ((:=))

import Api.Http exposing (..)
import Api.Models exposing (CategoryId, Category, Product)
import Categories.Messages exposing (..)
import Categories.Models exposing (CategoryData)


fetchAll : Cmd Msg
fetchAll = get CategoriesEndpoint categoriesDecoder FetchAllFail FetchAllDone


fetchOne : CategoryId -> Cmd Msg
fetchOne categoryId =
    get (CategoryEndpoint categoryId) categoryDecoder FetchOneFail FetchOneDone


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
