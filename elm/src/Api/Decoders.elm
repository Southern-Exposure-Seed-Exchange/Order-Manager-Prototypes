module Api.Decoders exposing (..)

import Json.Decode as Decode exposing ((:=))
import Api.Models exposing (ApiError, Category, Product, ProductVariant)


apiErrorDecoder : Decode.Decoder (List ApiError)
apiErrorDecoder =
    let
        errorDecoder =
            Decode.object2 ApiError
                ("source" := Decode.string)
                ("detail" := Decode.string)
    in
        "errors" := Decode.list errorDecoder


categoryDecoder : Decode.Decoder Category
categoryDecoder =
    Decode.object4 Category
        ("id" := Decode.int)
        ("name" := Decode.string)
        ("description" := Decode.string)
        (Decode.maybe ("parent" := Decode.int))


productVariantDecoder : Decode.Decoder ProductVariant
productVariantDecoder =
    Decode.object6 ProductVariant
        ("id" := Decode.int)
        ("sku" := Decode.string)
        ("product" := Decode.int)
        ("price" := Decode.int)
        ("quantity" := Decode.int)
        ("weight" := Decode.int)


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
