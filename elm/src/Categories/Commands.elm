module Categories.Commands exposing (..)

import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode

import Api.Decoders exposing (categoryDecoder, productDecoder)
import Api.Encoders exposing (categoryEncoder)
import Api.Http exposing (..)
import Api.Models exposing (CategoryId, Category, Product, initialCategory)
import Categories.Messages exposing (..)
import Categories.Models exposing (CategoryData)


fetchAll : Cmd Msg
fetchAll =
    get CategoriesEndpoint categoriesDecoder FetchAllFail FetchAllDone


fetchOne : CategoryId -> Cmd Msg
fetchOne categoryId =
    get (CategoryEndpoint categoryId) categoriesDecoder FetchOneFail (FetchOneDone categoryId)


updateOne : Category -> Cmd Msg
updateOne category =
    put (CategoryEndpoint category.id) (categoriesEncoder category)
        ("category" := categoryDecoder) UpdateOneFail (UpdateOneDone category.id)


createOne : Category -> Cmd Msg
createOne category =
    post CategoriesEndpoint (categoriesEncoder category)
        ("category" := categoryDecoder) CreateOneFail CreateOneDone


deleteOne : CategoryId -> Cmd Msg
deleteOne categoryId =
    delete (CategoryEndpoint categoryId) (Decode.succeed categoryId) DeleteOneFail DeleteOneDone


categoriesDecoder : Decode.Decoder CategoryData
categoriesDecoder =
    Decode.object3 CategoryData
        ("category" := Decode.list categoryDecoder)
        ("product" := Decode.list productDecoder)
        (Decode.succeed initialCategory)


categoriesEncoder : Category -> Encode.Value
categoriesEncoder category =
    Encode.object
        [ ("category", categoryEncoder category) ]
