module Main where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, (.?), decodeJson)
import Data.Array (fromFoldable)
import Data.Either (Either(..))
import Data.List (List(Nil))
import Data.Maybe (Maybe)
import Data.MediaType.Common (applicationJSON)
import Network.HTTP.Affjax (AffjaxResponse, AJAX, affjax, defaultRequest)
import Network.HTTP.RequestHeader (RequestHeader(..))

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.Util (awaitBody, runHalogenAff)


data Query a
    = FetchCategories a


data Category = Category
    { id :: Int
    , name :: String
    , description :: String
    , parent :: Maybe Int
    }


instance decodeJsonCategory :: DecodeJson Category where
    decodeJson json = do
        obj <- decodeJson json
        id <- obj .? "id"
        name <- obj .? "name"
        description <- obj .? "description"
        parent <- obj .? "parent"
        pure $ Category { id, name, description, parent }


data CategoryData = CategoryData
    { categories :: List Category
    }


decodeCategoryData :: AffjaxResponse Json -> Either String CategoryData
decodeCategoryData res = do
        obj <- decodeJson res.response
        categories <- obj .? "category" >>= decodeJson
        pure $ CategoryData { categories }


type State =
    CategoryData


initialState :: State
initialState = CategoryData
    { categories: Nil
    }


type AppEffects e = H.HalogenEffects ( ajax :: AJAX | e )


ui :: forall e. H.Component State Query (Aff (AppEffects e))
ui = H.component { render, eval }
    where render :: State -> H.ComponentHTML Query
          render (CategoryData state) =
              HH.div_
                [ HH.h1_ [ HH.text "Categories" ]
                , HH.button
                    [ HE.onClick (HE.input_ FetchCategories) ]
                    [ HH.text "Fetch" ]
                , categoryTable state.categories
                ]
          categoryTable :: List Category -> H.ComponentHTML Query
          categoryTable categories =
              HH.table_
                [ HH.thead_
                    [ HH.tr_
                        [ HH.th_ [ HH.text "Name" ]
                        , HH.th_ [ HH.text "Categories" ]
                        , HH.th_ [ HH.text "Products" ]
                        ]
                    ]
                , HH.tbody_ $ fromFoldable $ map categoryRow categories
                ]
          categoryRow :: Category -> H.ComponentHTML Query
          categoryRow (Category category) =
              HH.tr_
                [ HH.td_ [ HH.text category.name ]
                , HH.td_ [ HH.text "0" ]
                , HH.td_ [ HH.text "0" ]
                ]
          eval :: Query ~> H.ComponentDSL State Query (Aff (AppEffects e))
          eval (FetchCategories next) = do
              result <- H.fromAff fetchCategories
              H.modify (\state -> result)
              pure next


fetchCategories :: forall e. Aff (ajax :: AJAX | e) CategoryData
fetchCategories = do
    result <- affjax $ defaultRequest
        { url = "http://localhost:3000/categories"
        , headers = [ ContentType applicationJSON, Accept applicationJSON ]
        }
    cats <- pure $ do
        decodeCategoryData result
    pure $ case cats of
        Right cs -> cs
        Left err -> CategoryData { categories: Nil }


main :: Eff (H.HalogenEffects (ajax :: AJAX)) Unit
main = runHalogenAff do
  body <- awaitBody
  H.runUI ui initialState body
