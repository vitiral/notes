module Main exposing (..)

import Html exposing (program)
import Messages exposing (Msg(..))
import Models exposing (Model, initialModel)
import View exposing (view)
import Update exposing (update)
import Players.Commands exposing (fetchAll)


init : (Model, Cmd Msg)
init =
    (initialModel, Cmd.map PlayersMsg fetchAll)


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- MAIN

main : Program Never Model Msg
main =
    program
      { init = init
      , view = view
      , update = update
      , subscriptions = subscriptions
      }
