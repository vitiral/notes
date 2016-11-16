import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode

main =
  Html.program
    { init = init "cats"
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL
type alias Model =
  { topic: String
  , gifUrl: String
  }

init : String -> (Model, Cmd Msg)
init topic = (Model topic "waiting.gif", Cmd.none)

-- UPDATE

type Msg =
  Moar

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  (model, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ h2 [] [ text model.topic ]
    , img [ src model.gifUrl ] []
    , button [ onClick Moar ] [ text "Moar Plz" ]
    ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
