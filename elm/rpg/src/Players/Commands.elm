module Players.Commands exposing (..)

import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Task
import Players.Models exposing (PlayerId, Player)
import Players.Messages exposing (..)

-- constants
fetchAllUrl = "http://localhost:4000/players"


-- COMMANDS

fetchAll : Cmd Msg
fetchAll =
  let
    request =
      Http.get fetchAllUrl collectionDecoder
  in
    Http.send NewPlayers request


-- DECODERS

collectionDecoder : Decode.Decoder (List Player)
collectionDecoder =
  Decode.list memberDecoder

memberDecoder : Decode.Decoder Player
memberDecoder =
  decode Player
    |> required "id" Decode.int
    |> required "name" Decode.string
    |> required "level" Decode.int
