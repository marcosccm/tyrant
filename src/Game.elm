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
import Enemies
import PlayerActions
import AnimationFrame
import Boundaries exposing (Boundaries)
import GameOverScreen
import Collision


type alias Model =
    { ship : Ship.Model
    , cannon : Cannon.Model
    , enemies : Enemies.Model
    , over : Bool
    }


boundaries : Boundaries
boundaries =
    Boundaries.init 800 800


init : Model
init =
    { ship = Ship.init
    , cannon = Cannon.init
    , enemies = Enemies.init
    , over = False
    }


type Msg
    = PlayerAction PlayerActions.Action
    | Tick Time


type PossibleCollision
    = ShipAndEnemy


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Tick delta ->
            case (detectCollisions model) of
                Just ShipAndEnemy ->
                    return { model | over = True }

                Nothing ->
                    let
                        newShip =
                            Ship.tick delta boundaries model.ship

                        newCannon =
                            Cannon.tick delta model.cannon

                        newEnemies =
                            Enemies.tick delta boundaries model.enemies
                    in
                        return <|
                            { model
                                | ship = newShip
                                , cannon = newCannon
                                , enemies = newEnemies
                            }

        PlayerAction playerAction ->
            let
                newShip =
                    Ship.processInput playerAction model.ship

                shipPosition =
                    Ship.center newShip

                newCannon =
                    Cannon.processInput playerAction model.cannon shipPosition
            in
                return <|
                    { model
                        | ship = newShip
                        , cannon = newCannon
                    }


detectCollisions : Model -> Maybe PossibleCollision
detectCollisions { ship, enemies } =
    if Collision.detect enemies ship then
        Just ShipAndEnemy
    else
        Nothing


return : Model -> ( Model, Cmd Msg )
return model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Sub.map PlayerAction PlayerActions.subscriptions
        ]


viewBoxSize : String
viewBoxSize =
    let
        { x, y } =
            boundaries
    in
        [ 0, 0, y, x ]
            |> List.map toString
            |> String.join " "


view : Model -> Svg Msg
view { ship, cannon, enemies, over } =
    if over then
        renderUniverse [ (GameOverScreen.view boundaries) ]
    else
        Ship.view ship
            :: (Cannon.view cannon)
            ++ (Enemies.view enemies)
            |> renderUniverse


renderUniverse : List (Svg a) -> Svg a
renderUniverse children =
    let
        { y, x } =
            boundaries
    in
        svg
            [ height (toString y)
            , width (toString x)
            , viewBox viewBoxSize
            ]
            children
