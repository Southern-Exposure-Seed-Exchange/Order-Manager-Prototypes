module Api.Http where

import Prelude (($), (<<<), Unit)
import Data.Either (Either(Left))
import Data.HTTP.Method (Method(..))
import Data.MediaType.Common (applicationJSON)
import Network.HTTP.Affjax (Affjax, AffjaxRequest, affjax, defaultRequest)
import Network.HTTP.Affjax.Response (class Respondable)
import Network.HTTP.RequestHeader (RequestHeader(..))


apiRequest :: Method -> String -> AffjaxRequest Unit
apiRequest method url =
    defaultRequest
        { method = Left method
        , url = url
        , headers = [ ContentType applicationJSON, Accept applicationJSON ]
        }


get :: forall a eff. (Respondable a) => String -> Affjax eff a
get =
    affjax <<< apiRequest GET


delete :: forall a eff. (Respondable a) => String -> Affjax eff a
delete =
    affjax <<< apiRequest DELETE
