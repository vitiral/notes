module Players.Messages exposing (..)

import Http
import Players.Models exposing (PlayerId, Player)


-- TODO: can refactor to remove the Fail
type Msg
  = NewPlayers (Result Http.Error (List Player))

