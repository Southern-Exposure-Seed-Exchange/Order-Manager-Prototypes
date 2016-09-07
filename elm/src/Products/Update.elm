module Products.Update exposing (..)

import Dict

import Api.Models exposing (ProductId)
import Products.Messages exposing (Msg(..))
import Products.Models exposing (ProductData)


update : Msg -> ProductData -> ( ProductData, Cmd Msg )
update msg model =
    case msg of
        FetchAllDone newModel ->
            ( { newModel | showSKUs = model.showSKUs }, Cmd.none )
        FetchAllFail _ ->
            ( model, Cmd.none )
        ToggleSKUs productId ->
            ( { model | showSKUs = toggleSKU model.showSKUs productId }, Cmd.none )
        ToggleAllSKUs ->
            ( { model | showSKUs = toggleAllSKUs model }, Cmd.none )


toggleSKU : Dict.Dict ProductId Bool -> ProductId -> Dict.Dict ProductId Bool
toggleSKU skus id =
    if Dict.member id skus then
        Dict.update id (Maybe.map not) skus
    else
        Dict.insert id True skus


toggleAllSKUs : ProductData -> Dict.Dict ProductId Bool
toggleAllSKUs model =
    let
        allShown =
            Dict.values model.showSKUs
                |> List.all (\v -> v == True)
                |> (&&) (not <| Dict.isEmpty model.showSKUs)
    in
       if allShown then
            Dict.map (\_ _ -> False) model.showSKUs
       else
            List.foldl (\p d -> Dict.insert p.id True d) model.showSKUs model.products
