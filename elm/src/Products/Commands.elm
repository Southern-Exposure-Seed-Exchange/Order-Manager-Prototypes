module Products.Commands exposing (..)

import Dict
import Json.Decode as Decode exposing ((:=))

import Api.Decoders exposing (categoryDecoder, productDecoder, productVariantDecoder)
import Api.Http exposing (..)
import Products.Messages exposing (..)
import Products.Models exposing (ProductData)


fetchAll : Cmd Msg
fetchAll =
    get ProductsEndpoint productsDecoder FetchAllFail FetchAllDone


productsDecoder : Decode.Decoder ProductData
productsDecoder =
    Decode.object4 ProductData
        ("product" := Decode.list productDecoder)
        ("productVariant" := Decode.list productVariantDecoder)
        ("category" := Decode.list categoryDecoder)
        (Decode.succeed Dict.empty)