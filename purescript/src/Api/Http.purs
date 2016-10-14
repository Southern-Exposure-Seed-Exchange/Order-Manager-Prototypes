module Api.Http where

import Prelude (($), (<<<), (<>), Unit, show)
import Data.Either (Either(Left))
import Data.HTTP.Method (Method(..))
import Data.MediaType.Common (applicationJSON)
import Network.HTTP.Affjax (Affjax, AffjaxRequest, affjax, defaultRequest)
import Network.HTTP.Affjax.Response (class Respondable)
import Network.HTTP.RequestHeader (RequestHeader(..))

import Api.Models (CategoryId)


-- | An `Endpoint` describes a backend URL that can be used to fetch/send data.
data Endpoint
    = CategoriesEndpoint
    | CategoryEndpoint CategoryId


-- | Convert an `Endpoint` into a string containing the correct URL for the
-- | request.
endpointToUrl :: Endpoint -> String
endpointToUrl endpoint =
    case endpoint of
        CategoriesEndpoint ->
            "/api/categories/"
        CategoryEndpoint id ->
            endpointToUrl CategoriesEndpoint <> show id


apiRequest :: Method -> String -> AffjaxRequest Unit
apiRequest method url =
    defaultRequest
        { method = Left method
        , url = url
        , headers = [ ContentType applicationJSON, Accept applicationJSON ]
        }


get :: forall a eff. (Respondable a) => Endpoint -> Affjax eff a
get =
    affjax <<< apiRequest GET <<< endpointToUrl


delete :: forall a eff. (Respondable a) => Endpoint -> Affjax eff a
delete =
    affjax <<< apiRequest DELETE <<< endpointToUrl
