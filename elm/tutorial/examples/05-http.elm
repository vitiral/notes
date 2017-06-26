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
init topic = 
  (Model topic "waiting.jpe", getRandomGif topic)

-- UPDATE

type Msg =
  Moar
  | NewGif (Result Http.Error String)
  | NewTopic String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    -- request a new url for future calls to update
    Moar ->
      (model, getRandomGif model.topic)

    NewTopic topic ->
      ({ model | topic = topic }, getRandomGif topic)

    -- if we got a new url, replace it
    NewGif (Ok newUrl) ->
      ({ model | gifUrl = newUrl }, Cmd.none)

    -- do nothing on errors
    NewGif (Err _) ->
      (model, Cmd.none)

getRandomGif : String -> Cmd Msg
getRandomGif topic =
  let
    url = 
      "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic

    request = 
      Http.get url decodeGifUrl
  in 
    Http.send NewGif request

-- decode a url request into a string
decodeGifUrl : Decode.Decoder String
decodeGifUrl =
  Decode.at ["data", "image_url"] Decode.string

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ h2 [] [ text model.topic ]
    --, div [] [ input [ onInput NewTopic ] [] ]
    , div [] [ select [ onInput NewTopic ] 
      [ option [ value "cats" ] [ text "cats" ]
      , option [ value "pirates" ] [ text "pirates" ]
      ] 
    ]
    , div [] [ button [ onClick Moar ] [ text "Moar Plz" ] ]
    , div [] [ img [ src model.gifUrl ] [] ]
    ]


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
