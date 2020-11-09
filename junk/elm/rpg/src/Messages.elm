module Messages exposing (..)

import Players.Messages
import Players.Models exposing (PlayerId)

type Route
  = PlayersRoute
  | PlayerRoute PlayerId
  | NotFoundRoute
  

type Msg
  = PlayersMsg Players.Messages.Msg
  | RouteChange Route
