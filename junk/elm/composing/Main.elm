module Main exposing (..)

import Html exposing (Html, program)
import Widget


-- MODEL

type alias Model =
  { widgetModel : Widget.Model
  }

initialModel : Model
initialModel =
  { widgetModel = Widget.initialModel
  }

init : (Model, Cmd Msg)
init =
  ( initialModel, Cmd.none )


-- MESSAGES

type Msg
  = WidgetMsg Widget.Msg

-- VIEW

view : Model -> Html Msg
view model =
  Html.div []
    [ Html.map WidgetMsg (Widget.view model.widgetModel)
    ]

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    WidgetMsg subMsg -> 
      let
        ( updatedWidgetModel, widgetCmd ) =
          Widget.update subMsg model.widgetModel
      in
        ( { model | widgetModel = updatedWidgetModel }
        , Cmd.map WidgetMsg widgetCmd 
        )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- APP

main : Program Never Model Msg
main =
  program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
