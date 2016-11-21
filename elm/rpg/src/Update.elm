module Update exposing (..)

import Messages exposing (Msg(..))
import Models exposing (Model)
import Players.Update

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    PlayersMsg subMsg -> 
      let
        ( updatedPlayers, subCmd ) =
          Players.Update.update subMsg model.players
      in
        ( { model | players = updatedPlayers }
        , Cmd.map PlayersMsg subCmd 
        )
    -- TODO: do something here
    PlayersRoute ->
      ( model, Cmd.none )
    PlayerRoute id ->
      ( model, Cmd.none )
    NotFoundRoute ->
      ( model, Cmd.none )
