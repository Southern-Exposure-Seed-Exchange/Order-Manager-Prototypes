module App (omApp) where

import Web.Spock.Safe           (subcomponent)

import Handlers.Categories
import Types

omApp :: OM a
omApp = subcomponent "categories" categoryHandlers
