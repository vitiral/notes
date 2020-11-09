module Players.Update exposing (..)

import Navigation

import Http exposing (Error(..))
import Players.Messages exposing (Msg(..))
import Players.Models exposing (Player, PlayerId)
import Players.Commands exposing (save)

update : Msg -> List Player -> ( List Player, Cmd Msg )
update msg players =
  case msg of
    NewPlayers (Ok newPlayers) ->
      ( newPlayers, Cmd.none )

    NewPlayers (Err err) -> case err of
      BadPayload desc resp ->
        ( players, Navigation.newUrl ("error/" ++ desc ))

      BadUrl _ ->
        ( players, Navigation.newUrl "error/url" )

      Timeout ->
        ( players, Navigation.newUrl "error/timeout" )

      NetworkError ->
        let 
          _ =Debug.log "Network error" err
        in
          ( players, Navigation.newUrl "error/network" )

      BadStatus _ ->
        ( players, Navigation.newUrl "error/status" )

    ShowPlayers ->
      ( players, Navigation.newUrl "#players" )

    ShowPlayer id ->
      ( players, Navigation.newUrl ("#players/" ++ (toString id)) )

    ChangeLevel id amount ->
      ( players
      , Cmd.batch (changeLevelCommands id amount players)
      )

    SavePlayer result -> case result of
      Err err ->
        -- TODO: do something else here
        ( players, Navigation.newUrl "error"  )

      Ok newPlayer ->
        ( updatePlayer newPlayer players
        , Cmd.none )


changeLevelCommands playerId howMuch players =
  let
    cmdForPlayer existingPlayer =
      if existingPlayer.id == playerId then
        save { existingPlayer | level = existingPlayer.level + howMuch }
      else
        Cmd.none
  in
    List.map cmdForPlayer players

updatePlayer updatedPlayer players =
  let
    select existingPlayer =
      if existingPlayer.id == updatedPlayer.id then
        updatedPlayer
      else
        existingPlayer
  in
    List.map select players
