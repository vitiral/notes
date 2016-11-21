module Messages exposing (..)

import Players.Messages
import Players.Models exposing (PlayerId)

type Msg
  = PlayersMsg Players.Messages.Msg
  | PlayersRoute
  | PlayerRoute PlayerId
  | NotFoundRoute
