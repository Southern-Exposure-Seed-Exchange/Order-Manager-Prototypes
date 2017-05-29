module Utils exposing (..)

{-| This module contains general utility functions that are used throughout
the application.

# Maybes
@docs isNothing

# Lists
@docs replaceBy, replaceAllById, filterBy, filterById, getById

# Html Events
@docs onChange, onClickNoDefault

# Currency
@docs toDollars

-}

import Data.Decimal as Decimal
import Html
import Html.Events exposing (defaultOptions)
import Json.Decode


{-| Determine if a `Maybe` type has a value of `Nothing`.
-}
isNothing : Maybe a -> Bool
isNothing maybeValue =
    case maybeValue of
        Nothing ->
            True

        Just _ ->
            False


{-| Replace or insert an item in a list using a selector function.
-}
replaceBy : (a -> b) -> a -> List a -> List a
replaceBy selector item items =
    case items of
        [] ->
            [ item ]

        x :: xs ->
            if selector item == selector x then
                item :: xs
            else
                x :: replaceBy selector item xs


{-| Replace or insert list values from an attribute of two items.
-}
replaceAllById : a -> a -> (a -> List { b | id : c }) -> List { b | id : c }
replaceAllById oldModel newModel selector =
    List.foldl (replaceBy .id) (selector oldModel) (selector newModel)


{-| Filter a list of items by some predicate and an expected value.
-}
filterBy : (a -> b) -> b -> List a -> List a
filterBy selector value items =
    items |> List.filter (\i -> selector i == value)


{-| Filter a list of items where some attribute matches the `id` of a value.
-}
filterById : (a -> b) -> { c | id : b } -> List a -> List a
filterById selector value =
    filterBy selector (.id value)


{-| Attempt to find an item in a list given an `id` value.
-}
getById : List { b | id : a } -> a -> Maybe { b | id : a }
getById items id =
    items |> filterBy .id id |> List.head


{-| An event handler, firing on `change` events and returning a `targetValue`.
-}
onChange : (String -> msg) -> Html.Attribute msg
onChange msg =
    Html.Events.on "change" (Json.Decode.map msg Html.Events.targetValue)


{-| A `click` event handler that prevents default functionality
-}
onClickNoDefault : msg -> Html.Attribute msg
onClickNoDefault msg =
    Html.Events.onWithOptions "click"
        { defaultOptions | preventDefault = True }
        (Json.Decode.succeed msg)

{-| Convert an Integer representing Cents to a String representing Dollars.
-}
toDollars : Int -> String
toDollars =
    let
        oneHundreth =
            case Decimal.fromString "0.01" of
                Nothing ->
                    Debug.crash "Utils.toDollars: Error Converting 1/100 to Decimal."
                Just x ->
                    x

    in
        Decimal.fromInt >> Decimal.mul oneHundreth >> Decimal.toString
