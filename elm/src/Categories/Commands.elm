module Categories.Commands exposing (..)

import Json.Decode as Decode exposing ((:=))

import Api.Decoders exposing (categoryDecoder, productDecoder)
import Api.Http exposing (..)
import Api.Models exposing (CategoryId, Category, Product)
import Categories.Messages exposing (..)
import Categories.Models exposing (CategoryData)


fetchAll : Cmd Msg
fetchAll =
    get CategoriesEndpoint categoriesDecoder FetchAllFail FetchAllDone


fetchOne : CategoryId -> Cmd Msg
fetchOne categoryId =
    get (CategoryEndpoint categoryId) categoriesDecoder FetchOneFail FetchOneDone


categoriesDecoder : Decode.Decoder CategoryData
categoriesDecoder =
    Decode.object2 CategoryData
        ("category" := Decode.list categoryDecoder)
        ("product" := Decode.list productDecoder)
