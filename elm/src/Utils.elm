module Utils exposing (..)


replaceBy : (a -> b) -> a -> List a -> List a
replaceBy selector item items =
    case items of
        [] ->
            [ item ]
        x::xs ->
            if selector item == selector x then
                item :: xs
            else
                x :: replaceBy selector item xs


replaceAllById : a -> a -> (a -> List { b | id : c }) -> List { b | id : c }
replaceAllById oldModel newModel selector =
    List.foldl (replaceBy .id) (selector oldModel) (selector newModel)


filterBy : (a -> b) -> b -> List a -> List a
filterBy selector value items =
    items |> List.filter (\i -> selector i == value)


getById : List { b | id : a } -> a -> Maybe { b | id : a }
getById items id = items |> filterBy .id id |> List.head
