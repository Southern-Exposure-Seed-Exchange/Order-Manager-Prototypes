module Api.Encoders exposing (..)

import Json.Encode as Encode exposing (Value)

import Api.Models exposing (Category)


categoryEncoder : Category -> Value
categoryEncoder category =
    Encode.object <|
        [ ("name", Encode.string category.name)
        , ("description", Encode.string category.description)
        , encodeMaybe category.parent "parent" Encode.int
        ] ++ (encodeId category.id)


encodeId : Int -> List (String, Value)
encodeId id =
    case id of
        0 ->
            []
        _ ->
            [ ("id", Encode.int id) ]


encodeMaybe : Maybe a -> String -> (a -> Value) -> (String, Value)
encodeMaybe maybeValue name encoder =
    case maybeValue of
        Nothing ->
            (name, Encode.null)
        Just value ->
            (name, encoder value)
