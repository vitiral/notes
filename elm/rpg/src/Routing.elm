module Routing exposing (..)

import String
import Navigation
import UrlParser as UP exposing ((</>))
import Players.Models exposing (PlayerId)


type Route
  = PlayersRoute
  | PlayerRoute PlayerId
  | NotFoundRoute


route : UP.Parser (Route -> a) a
route =
  UP.oneOf
    [ UP.map PlayersRoute (UP.s "")
    , UP.map PlayerRoute (UP.s "players" </> UP.int)
    , UP.map PlayersRoute (UP.s "players")
    ]


hashParser : Navigation.Location -> Maybe Route
hashParser location =
  UP.parseHash route location

--parser : Navigation.Parser (Result String Route)
--parser =
--  Navigation.makeParser hashParser
