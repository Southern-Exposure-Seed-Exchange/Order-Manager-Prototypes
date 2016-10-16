module Api.Errors exposing (..)

import Json.Decode as Decode exposing ((:=), decodeString)


type alias ApiError =
    { source : String
    , detail : String
    }


apiErrorDecoder : Decode.Decoder (List ApiError)
apiErrorDecoder =
    let
        errorDecoder =
            Decode.object2 ApiError
                ("source" := Decode.string)
                ("detail" := Decode.string)
    in
        "errors" := Decode.list errorDecoder


parseErrors : (ApiError -> a -> a) -> a -> String -> a
parseErrors assignError initialErrors data =
    let
        decoded =
            decodeString apiErrorDecoder data
    in
        case decoded of
            Ok apiErrors ->
                List.foldl assignError initialErrors apiErrors

            Err _ ->
                initialErrors
