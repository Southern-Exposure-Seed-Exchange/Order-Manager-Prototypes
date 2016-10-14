module Utils where

import Prelude (($), bind, (==))
import Data.Foldable (foldl)
import Data.List (List, findIndex, updateAt, (:))
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
