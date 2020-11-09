module View exposing (..)

import Html exposing (Html, div, text)
import Messages exposing (Msg(..), Route(..))
import Models exposing (Model)
import Players.List
import Players.Edit
import Players.Models exposing (PlayerId)


view : Model -> Html Msg
view model =
  div []
    [ page model ]


page : Model -> Html Msg
page model =
  case model.route of
    PlayersRoute ->
      Html.map PlayersMsg (Players.List.view model.players)

    PlayerRoute id ->
      playerEditPage model id

    NotFoundRoute ->
      notFoundView

playerEditPage : Model -> PlayerId -> Html Msg
playerEditPage model id =
  let
    maybePlayer =
      model.players
        |> List.filter (\player -> player.id == id)
        |> List.head
  in
      case maybePlayer of
        Just player ->
          Html.map PlayersMsg (Players.Edit.view player)

        Nothing ->
          notFoundView

notFoundView : Html a
notFoundView =
  div []
    [ text "Route Not Found"
    ]

