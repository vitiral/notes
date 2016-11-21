module Messages exposing (..)

import Players.Messages

type Msg
  = PlayersMsg Players.Messages.Msg
  --| PlayersRoute
  --| PlayerRoute PlayerId
  --| NotFoundRoute
