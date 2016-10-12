module Update where

import Prelude
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, mapEffects)

import Messages (Msg(..))
import Model (Model)
import Categories.Update as CatUpdate


update :: Msg -> Model -> EffModel Model Msg (ajax :: AJAX)
update (CategoriesMsg subMsg) model =
    mapEffects CategoriesMsg $ CatUpdate.update subMsg model
