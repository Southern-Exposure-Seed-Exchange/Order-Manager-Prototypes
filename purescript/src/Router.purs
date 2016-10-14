module Router where

import Prelude ((<*), (<$>), (<$), ($), (<>), (*>), show)
import Control.Alt ((<|>))
import Data.Maybe (fromMaybe)
import Pux.Router (router, lit, int, end)


data Route
    = Home
    | Categories
    | CategoryDetail Int
    | NotFound


match :: String -> Route
match url = fromMaybe NotFound $ router url $
    Home <$ end
    <|>
    Categories <$ (lit "categories") <* end
    <|>
    CategoryDetail <$> (lit "categories" *> int) <* end


reverse :: Route -> String
reverse route =
    case route of
        Home ->
            "/"
        Categories ->
            "/categories"
        CategoryDetail id ->
            "/categories/" <> show id
        NotFound ->
            "/404"
