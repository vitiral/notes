module Players.Commands exposing (..)

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Task
import Players.Models exposing (PlayerId, Player)
import Players.Messages exposing (..)

-- WTF

defaultSettings =
  { timeout = 0
  , onStart = Nothing
  , onProgress = Nothing
  , desiredResponseType = Nothing
  , withCredentials = False
  }

-- constants
url = "http://localhost:4000"
playersUrl = url ++ "/players"
fetchAllUrl = playersUrl

saveUrl playerId =
  playersUrl ++ "/" ++ (toString playerId)

-- COMMANDS

fetchAll : Cmd Msg
fetchAll =
  let
    request =
      Http.get fetchAllUrl collectionDecoder
  in
    Http.send NewPlayers request


save : Player -> Cmd Msg
save player = 
  let
    body = Http.jsonBody (memberEncoded player)

    request = Http.request
      { method = "PATCH"
      , headers = [ Http.header "Content-Type" "application/json" ]
      , url = saveUrl player.id
      , body = body
      , expect = Http.expectJson memberDecoder
      , timeout = Nothing
      , withCredentials = False
      }
  in
    Http.send SavePlayer request


-- ENCODER

memberEncoded : Player -> Encode.Value
memberEncoded player =
  let
    attrs =
      [ ( "id", Encode.int player.id )
      , ( "name", Encode.string player.name )
      , ( "level", Encode.int player.level)
      ]
  in
    Encode.object attrs

  

-- DECODERS

collectionDecoder : Decode.Decoder (List Player)
collectionDecoder =
  Decode.list memberDecoder

memberDecoder : Decode.Decoder Player
memberDecoder =
  Decode.map3 Player
    (Decode.field "id" (Decode.map toFuckingId Decode.string))
    (Decode.field "name" Decode.string)
    (Decode.field "level" Decode.int)
  --decode Player
    ----|> required "id" (Decode.customDecoder Decode.string String.toInt)
    --|> required "name" Decode.string
    --|> required "level" Decode.int

-- TODO: this should do the right thing...
toFuckingId strId =
  let
    id = String.toInt strId
  in
    case id of
      Ok id -> id
      Err _ -> -1




--decodeId =
--  Decode string
--    |> andThen
