module Players.Update exposing (..)

import Players.Messages exposing (Msg(..))
import Players.Models exposing (Player)

update : Msg -> List Player -> ( List Player, Cmd Msg )
update msg players =
  case msg of
    NewPlayers (Ok newPlayers) ->
      ( newPlayers, Cmd.none )

    NewPlayers (Err _) ->
      ( players, Cmd.none )

