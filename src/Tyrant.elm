module Tyrant exposing (..)

import Debug
import String
import List
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html.App as App
import Ship
import Laser
import Keyboard exposing (KeyCode)


type alias Model =
    { ship : Ship.Model
    , shots : List Laser.Model
    }


gameSize : ( Float, Float )
gameSize =
    ( 800, 800 )


initialModel : Model
initialModel =
    let
        ( xLimit, yLimit ) =
            gameSize
    in
        { ship = Ship.startingShip ( xLimit, yLimit )
        , shots = []
        }


type Msg
    = ActOnShip Ship.Msg
    | ActOnLasers Laser.Msg
    | Shoot
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Shoot ->
            let
                nextShot =
                    Laser.shoot (Ship.center model.ship) 800
            in
                ( { model | shots = nextShot :: model.shots }, Cmd.none )

        ActOnLasers laserAction ->
            let
                newShots =
                    List.map (Laser.update laserAction) model.shots
            in
                ( { model | shots = newShots }, Cmd.none )

        ActOnShip shipAction ->
            let
                newShip =
                    Ship.update shipAction model.ship
            in
                ( { model | ship = newShip }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Sub.map ActOnShip Ship.subscriptions
        , Sub.map ActOnLasers Laser.subscriptions
        , Keyboard.downs keyDown
        ]


keyDown : KeyCode -> Msg
keyDown keyCode =
    case (Debug.log "keyCode" keyCode) of
        32 ->
            Shoot

        _ ->
            NoOp


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
view { ship, shots } =
    Ship.view ship
        :: List.map Laser.view shots
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
