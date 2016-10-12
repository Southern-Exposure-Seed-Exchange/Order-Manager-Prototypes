module Api.Http where

import Prelude (($))
import Data.Either (Either(Left))
import Data.HTTP.Method (Method(..))
import Data.MediaType.Common (applicationJSON)
import Network.HTTP.Affjax (Affjax, affjax, defaultRequest)
import Network.HTTP.Affjax.Response (class Respondable)
import Network.HTTP.RequestHeader (RequestHeader(..))


get :: forall a eff. (Respondable a) => String -> Affjax eff a
get url =
    affjax $ defaultRequest
        { method = Left GET
        , url = url
        , headers = [ ContentType applicationJSON, Accept applicationJSON ]
        }
