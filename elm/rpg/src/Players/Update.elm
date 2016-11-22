module Players.Update exposing (..)

import Navigation

import Players.Messages exposing (Msg(..))
import Players.Models exposing (Player)

update : Msg -> List Player -> ( List Player, Cmd Msg )
update msg players =
  case msg of
    NewPlayers (Ok newPlayers) ->
      ( newPlayers, Cmd.none )

    NewPlayers (Err _) ->
      ( players, Cmd.none )

    ShowPlayers ->
      ( players, Navigation.newUrl "#players" )

    ShowPlayer id ->
      ( players, Navigation.newUrl ("#players/" ++ (toString id)) )

    ChangeLevel id change ->
      -- TODO: do something here
      ( players, Cmd.none)
  
    SavePlayer result -> case result of
      Err err ->
        -- TODO: do something here
        ( players, Cmd.none)

      Ok newPlayer ->
        ( List.map (\p -> if p.id == newPlayer.id then newPlayer else p) players
        , Cmd.none )
