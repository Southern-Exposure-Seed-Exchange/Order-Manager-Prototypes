module Router where

import Prelude ((<*), (<$>), (<$), ($))
import Control.Alt ((<|>))
import Data.Maybe (fromMaybe)
import Pux.Router (router, lit, end)


data Route
    = Home
    | Categories
    | NotFound


match :: String -> Route
match url = fromMaybe NotFound $ router url $
    Home <$ end
    <|>
    Categories <$ (lit "categories") <* end


reverse :: Route -> String
reverse route =
    case route of
        Home ->
            "/"
        Categories ->
            "categories"
        NotFound ->
            "404"
