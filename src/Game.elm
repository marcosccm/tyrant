module Game
    exposing
        ( Model
        , subscriptions
        , update
        , view
        , init
        )

import Debug
import String
import List
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time)
import Ship
import Cannon
import PlayerActions
import AnimationFrame


type alias Model =
    { ship : Ship.Model
    , cannon : Cannon.Model
    }


gameSize : ( Float, Float )
gameSize =
    ( 800, 800 )


init : Model
init =
    let
        ( xLimit, yLimit ) =
            gameSize
    in
        { ship = Ship.startingShip ( xLimit, yLimit )
        , cannon = Cannon.init
        }


type Msg
    = PlayerAction PlayerActions.Action
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Tick timeDelta ->
            let
                newShip =
                    Ship.tick timeDelta model.ship

                newCannon =
                    Cannon.tick timeDelta model.cannon
            in
                ( { model | ship = newShip, cannon = newCannon }, Cmd.none )

        PlayerAction playerAction ->
            let
                newShip =
                    Ship.processInput playerAction model.ship

                shipPosition =
                    Ship.center newShip

                newCannon =
                    Cannon.processInput playerAction model.cannon shipPosition
            in
                ( { model | ship = newShip, cannon = newCannon }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Sub.map PlayerAction PlayerActions.subscriptions
        ]


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
view { ship, cannon } =
    Ship.view ship
        :: Cannon.view cannon
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
