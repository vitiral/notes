import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

main = 
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL

type alias Model = Int

-- set the starting value of model globally
model: Model
model =
  0


-- UPDATE

-- union (called enum elsewhere)
type Msg = Increment | Decrement

-- function update takes a message and a model and
-- creates an action
update : Msg -> Model -> Model
update msg model = 
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1

-- VIEW

view : Model -> Html Msg
view model = 
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (toString model) ]
    , button [ onClick Increment ] [ text "+" ]
    ]

