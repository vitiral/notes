module Main exposing (..)

import Navigation
--import Html exposing (program)
import Messages exposing (Msg(..), Route)
import Models exposing (Model, initialModel)
import View exposing (view)
import Update exposing (update)
import Players.Commands exposing (fetchAll)
import Routing



init : Navigation.Location -> (Model, Cmd Msg)
init location =
    (initialModel (Routing.router location)
    , Cmd.map PlayersMsg fetchAll )


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- MAIN

--main : Program Never Model Msg
main =
    Navigation.program Routing.routerMsg
      { init = init
      , view = view
      , update = update
      , subscriptions = subscriptions
      }
