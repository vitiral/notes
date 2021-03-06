module Models exposing (..)

import Messages exposing (Route)
import Players.Models exposing (Player)
import Routing

type alias Model =
  { players: List Player
  , route: Route
  }

initialModel : Route -> Model
initialModel route =
  { players = []
  , route = route
  }
