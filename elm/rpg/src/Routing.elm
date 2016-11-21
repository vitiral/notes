module Routing exposing (..)

import String
import Navigation
import UrlParser as UP exposing ((</>))
import Messages exposing (Msg(..), Route(..))


route : UP.Parser (Route -> a) a
route =
  UP.oneOf
    [ UP.map PlayersRoute (UP.s "")
    , UP.map PlayerRoute (UP.s "players" </> UP.int)
    , UP.map PlayersRoute (UP.s "players")
    ]


parser : Navigation.Location -> Msg
parser location =
  case UP.parseHash route location of
    Just route -> RouteChange route
    Nothing -> RouteChange NotFoundRoute
