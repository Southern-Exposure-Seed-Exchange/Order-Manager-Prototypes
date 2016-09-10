module Utils exposing (..)


replaceBy : (a -> b) -> a -> List a -> List a
replaceBy selector item =
    let
        itemValue =
            selector item
    in
        List.map (\listItem -> if selector listItem == itemValue then item else listItem)


filterBy : (a -> b) -> b -> List a -> List a
filterBy selector value items =
    items |> List.filter (\i -> selector i == value)


getById : List { b | id : a } -> a -> Maybe { b | id : a }
getById items id = items |> filterBy .id id |> List.head
