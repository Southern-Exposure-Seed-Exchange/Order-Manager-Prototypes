module Api.Encoders exposing (..)

import Json.Encode as Encode exposing (Value)
import Api.Models exposing (Category, Product)


categoryEncoder : Category -> Value
categoryEncoder category =
    Encode.object <|
        [ ( "name", Encode.string category.name )
        , ( "description", Encode.string category.description )
        , encodeMaybe category.parent "parent" Encode.int
        ]
            ++ (encodeId category.id)


productEncoder : Product -> Value
productEncoder product =
    Encode.object <|
        [ ( "name", Encode.string product.name )
        , ( "description", Encode.string product.description )
        , ( "category", Encode.int product.category )
        , ( "isActive", Encode.bool product.isActive )
        , ( "isOrganic", Encode.bool product.isOrganic )
        , ( "isHeirloom", Encode.bool product.isHeirloom )
        , ( "isSouthEast", Encode.bool product.isSouthEast )
        ]
            ++ (encodeId product.id)


encodeId : Int -> List ( String, Value )
encodeId id =
    case id of
        0 ->
            []

        _ ->
            [ ( "id", Encode.int id ) ]


encodeMaybe : Maybe a -> String -> (a -> Value) -> ( String, Value )
encodeMaybe maybeValue name encoder =
    case maybeValue of
        Nothing ->
            ( name, Encode.null )

        Just value ->
            ( name, encoder value )
