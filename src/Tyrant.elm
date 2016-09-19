module Tyrant exposing (..)

import Debug
import String
import List
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html.App as App
import Ship


type alias Model =
    { ship : Ship.Model }


gameSize : ( Float, Float )
gameSize =
    ( 800, 800 )


initialModel : Model
initialModel =
    let
        ( xLimit, yLimit ) =
            gameSize
    in
        { ship = Ship.startingShip ( xLimit, yLimit ) }


type Msg
    = ActOnShip Ship.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        ActOnShip shipAction ->
            let
                newShip =
                    Ship.update shipAction model.ship
            in
                ( { model | ship = newShip }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.map ActOnShip Ship.subscriptions


viewBoxSize : String
viewBoxSize =
    let
        ( xLimit, yLimit ) =
            gameSize
    in
        [ 0, 0, yLimit, xLimit ]
            |> List.map toString
            |> String.join " "


view : Model -> Svg Msg
view { ship } =
    [ Ship.view ship ]
        |> renderUniverse


renderUniverse : List (Svg a) -> Svg a
renderUniverse children =
    let
        ( xLimit, yLimit ) =
            gameSize
    in
        svg
            [ height (toString yLimit)
            , width (toString xLimit)
            , viewBox viewBoxSize
            ]
            children


main =
    App.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
