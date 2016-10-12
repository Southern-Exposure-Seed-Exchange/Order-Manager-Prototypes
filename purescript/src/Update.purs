module Update where

import Prelude
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, mapEffects, mapState)

import Categories.Update as CatUpdate
import Messages (Msg(..))
import Model (Model, updateModel, fromModel, class SubModel)


update :: Msg -> Model -> EffModel Model Msg (ajax :: AJAX)
update (CategoriesMsg subMsg) model =
    callNestedUpdate CategoriesMsg subMsg model CatUpdate.update


callNestedUpdate :: forall a b eff. SubModel a
                 => (b -> Msg) -> b -> Model -> (b -> a -> EffModel a b eff)
                 -> EffModel Model Msg eff
callNestedUpdate parentMsg msg model updateFunc =
    mapState (updateModel model) <<< mapEffects parentMsg <<< updateFunc msg $ fromModel model
