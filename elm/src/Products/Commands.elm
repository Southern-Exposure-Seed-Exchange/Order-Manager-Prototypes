module Products.Commands exposing (..)

import Dict
import Json.Decode as Decode exposing ((:=))
import Api.Decoders exposing (categoryDecoder, productDecoder, productVariantDecoder)
import Api.Http exposing (..)
import Api.Models exposing (Product, ProductId, initialProduct)
import Products.Form exposing (initialErrors)
import Products.Messages exposing (..)
import Products.Models exposing (ProductData)


fetchAll : Cmd Msg
fetchAll =
    get ProductsEndpoint productsDecoder FetchAllFail FetchAllDone


fetchOne : ProductId -> Cmd Msg
fetchOne id =
    get (ProductEndpoint id) productsDecoder FetchOneFail (FetchOneDone id)


deleteOne : ProductId -> Cmd Msg
deleteOne id =
    delete (ProductEndpoint id) (Decode.succeed id) DeleteOneFail DeleteOneDone


productsDecoder : Decode.Decoder ProductData
productsDecoder =
    Decode.object6 ProductData
        ("product" := Decode.list productDecoder)
        ("productVariant" := Decode.list productVariantDecoder)
        ("category" := Decode.list categoryDecoder)
        (Decode.succeed Dict.empty)
        (Decode.succeed initialProduct)
        (Decode.succeed initialErrors)
