module Routing exposing (..)

import String
import Navigation
import UrlParser as UP exposing ((</>))
import Messages exposing (Msg(..), Route(..))


-- parser : parses a url and converts to a Route
parser : UP.Parser (Route -> a) a
parser =
  UP.oneOf
    [ UP.map PlayersRoute (UP.s "")
    , UP.map PlayerRoute (UP.s "players" </> UP.int)
    , UP.map PlayersRoute (UP.s "players")
    ]


-- routes a location object to it's Route
router : Navigation.Location -> Route
router location =
  case UP.parseHash parser location of
    Just route -> route
    Nothing -> NotFoundRoute

-- convert a location into a Msg
routerMsg : Navigation.Location -> Msg
routerMsg location =
  RouteChange (router location)
