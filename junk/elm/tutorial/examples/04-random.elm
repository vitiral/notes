import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random

{- app that "rolls the dice", producing a random number between 1 and 6 
-}

main = Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

-- MODEL

type alias Model =
  { dieFace : Int
  }

init : (Model, Cmd Msg)
init =
  (Model 1, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text (toString model.dieFace) ]
    , button [ onClick Roll ] [ text "Roll" ]
    ]

-- UPDATE

seed = Random.initialSeed 10

type Msg =
  Roll
  | NewFace Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    {- Random.generate's signature is
    Random.generate : (a -> msg) -> Random.Generator a -> Cmd msg

    Here we call it with the function NewFace (signature a -> msg)
    AND we call it with (Random.int 1 6) which is itself a generator (Random.Generator a)
    ... and then that we get a Cmd msg...?

    I'm actualy not sure after that, it seems like we end up with Cmd Msg as the value...
    somehow. I think Random.generate will actually CALL the value...?

    No... generate takes a function and a generator and actually generates a value...

    It looks like the upper framework knows how to deal with the generator that was
    returned? I don't understand
    -}
    Roll ->
      (model, Random.generate NewFace <| Random.int 1 6)

    NewFace newFace ->
      ({model | dieFace = newFace }, Cmd.none)

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
