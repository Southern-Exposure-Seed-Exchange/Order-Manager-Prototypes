module Main where

import Prelude hiding (div)
import Control.Monad.Aff (attempt)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.?))
import Data.Array (fromFoldable)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Data.List (List(..), length, filter)
import Data.Maybe (Maybe, maybe)
import Data.MediaType.Common (applicationJSON)
import Network.HTTP.Affjax (AJAX, affjax, defaultRequest)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Signal.Channel (CHANNEL)

import Pux (renderToDOM, start, EffModel, noEffects)
import Pux.Html (Html, td, text, tr, tbody, th, thead, table, h1, div, button)
import Pux.Html.Events (onClick)


data Msg
    = FetchCategories
    | ReceiveCategories (Either String (List Category))

type CategoryId = Int

data Category = Category
    { id :: CategoryId
    , name :: String
    , description :: String
    , parent :: Maybe CategoryId
    }

instance decodeJsonCategory :: DecodeJson Category where
    decodeJson json = do
        obj <- decodeJson json
        id <- obj .? "id"
        name <- obj .? "name"
        description <- obj .? "description"
        parent <- obj .? "parent"
        pure $ Category { id, name, description, parent }

type ProductId = Int

type Product =
    { id :: ProductId
    , name :: String
    , description :: String
    , category :: CategoryId
    , isActive :: Boolean
    , isOrganic :: Boolean
    , isHeirloom :: Boolean
    , isSouthEast :: Boolean
    }

type Model =
    { categories :: List Category
    , products :: List Product
    }

init :: Model
init =
    { categories: Nil
    , products: Nil
    }

update :: Msg -> Model -> EffModel Model Msg (ajax :: AJAX)
update FetchCategories model =
    { state: model
    , effects: [ fetchCategories ]
    }
    where get url =
              affjax $ defaultRequest
                { method = Left GET
                , url = url
                , headers = [ ContentType applicationJSON, Accept applicationJSON ]
                }
          fetchCategories =
              do response <- attempt $ get "/api/categories"
                 let decode res = do
                        obj <- decodeJson res.response
                        list <- obj .? "category"
                        decodeJson list :: Either String (List Category)
                     categories = either (Left <<< show) decode response
                 pure $ ReceiveCategories categories
update (ReceiveCategories (Left s)) model =
    noEffects $ model
update (ReceiveCategories (Right categories)) model =
    noEffects $ model { categories = categories }

view :: Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Categories" ]
        , catTable model
        , button [onClick (const FetchCategories)] [ text "Fetch" ]
        ]

catTable :: Model -> Html Msg
catTable model =
    table []
        [ thead []
            [ tr []
                [ th [] [ text "Name" ]
                , th [] [ text "Categories" ]
                , th [] [ text "Products" ]
                ]
            ]
        , tbody [] <<< fromFoldable $ map (catRow model) model.categories
        ]

catRow :: Model -> Category -> Html Msg
catRow model category@(Category {name}) =
    tr []
        [ td [] [ text name ]
        , td [] [ text <<< show $ childCount category model.categories ]
        , td [] [ text <<< show $ productCount category model.products ]
        ]


childCount :: Category -> List Category -> Int
childCount (Category category) =
    length <<< filter (\(Category cat) -> maybe false ((==) category.id) cat.parent)


productCount :: Category -> List Product -> Int
productCount (Category category) =
    length <<< filter (\prod -> prod.category == category.id)


main :: Eff (err :: EXCEPTION, channel :: CHANNEL, ajax :: AJAX) Unit
main = do
    app <- start
        { initialState: init
        , update: update
        , view
        , inputs: []
        }
    renderToDOM "#app" app.html
