module Update where

import Prelude
import DOM (DOM)
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, mapEffects, mapState, noEffects, onlyEffects)

import Categories.Commands (fetchCategories, fetchCategory)
import Categories.Update as CatUpdate
import Classes (updateModel, fromModel, class SubModel)
import Messages (Msg(..))
import Model (Model)
import Router (Route(..))


update :: Msg -> Model -> EffModel Model Msg ( ajax :: AJAX, dom :: DOM )
update (PageView route) model =
    commandForRoute $ model { route = route }
update (CategoriesMsg subMsg) model =
    callNestedUpdate CategoriesMsg subMsg model CatUpdate.update


callNestedUpdate :: forall a b eff. SubModel a
                 => (b -> Msg) -> b -> Model -> (b -> a -> EffModel a b eff)
                 -> EffModel Model Msg eff
callNestedUpdate parentMsg msg model updateFunc =
    mapState (updateModel model) <<< mapEffects parentMsg <<< updateFunc msg $ fromModel model


commandForRoute :: forall e. Model -> EffModel Model Msg (ajax :: AJAX | e)
commandForRoute model =
    case model.route of
        Categories ->
            mapEffects CategoriesMsg $ onlyEffects model [ fetchCategories ]
        CategoryDetail id ->
            mapEffects CategoriesMsg $ onlyEffects model [ fetchCategory id ]
        Home ->
            noEffects model
        NotFound ->
            noEffects model
