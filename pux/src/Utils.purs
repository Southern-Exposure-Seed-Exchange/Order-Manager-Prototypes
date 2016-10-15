module Utils where

import Prelude (($), bind, (==), (/=), compare, class Ord)
import Data.Foldable (foldl)
import Data.List (List, findIndex, updateAt, (:), sortBy, filter)
import Data.Maybe (fromMaybe)

import Classes (class HasId, toId)


replaceAll :: forall a. HasId a => List a -> List a -> List a
replaceAll oldData newData =
    foldl (\acc i -> replaceById acc i) oldData newData


replaceById :: forall a. HasId a => List a -> a -> List a
replaceById list elem =
    fromMaybe (elem : list) $ do
        index <- findIndex (\e -> toId e == toId elem) list
        updateAt index elem list


deleteById :: forall a. HasId a => Int -> List a -> List a
deleteById id =
    filter (\element -> toId element /= id)


sortByField :: forall a b. Ord b => (a -> b) -> List a -> List a
sortByField selector =
    sortBy (\a1 a2 -> compare (selector a1) (selector a2))
