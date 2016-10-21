module Subscriptions exposing (subscriptions)

{-| This wraps subscriptions from individual models to match the global Msg
type.

@docs subscriptions

-}

import Categories.Commands exposing (categoryDeleteWasConfirmed)
import Categories.Messages
import Messages exposing (Msg(..))
import Models exposing (Model)
import Products.Commands exposing (productDeleteWasConfirmed)
import Products.Messages


{-| Subscribe to the Category & Product deletion confirmations.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ categoryDeleteWasConfirmed Categories.Messages.CategoryDeletionConfirmed
            |> Sub.map CategoriesMsg
        , productDeleteWasConfirmed Products.Messages.ProductDeletionConfirmed
            |> Sub.map ProductsMsg
        ]
