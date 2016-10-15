module Enemies
    exposing
        ( Model
        , view
        , tick
        , init
        , toCollisionRects
        )

import Svg exposing (path, Svg)
import Svg.Attributes exposing (..)
import String
import Time exposing (Time)
import PlayerActions as Action exposing (Action)
import Boundaries exposing (Boundaries)
import Collisions


type alias Ship =
    { posX : Float
    , posY : Float
    , width : Float
    , height : Float
    , xSpeed : Float
    , ySpeed : Float
    }


type alias Model =
    List Ship


toCollisionRects : Model -> List Collisions.Rect
toCollisionRects model =
    List.map toCollisionRect model


toCollisionRect : Ship -> Collisions.Rect
toCollisionRect { posX, posY, width, height } =
    { x = posX, y = posY, width = width, height = height }


init : Model
init =
    [ { posX = 800
      , posY = 100
      , width = 80
      , height = 40
      , xSpeed = -0.2
      , ySpeed = 0
      }
    ]


tick : Time -> Boundaries -> Model -> Model
tick delta boundaries model =
    List.map (tickShip delta boundaries) model


tickShip : Time -> Boundaries -> Ship -> Ship
tickShip delta boundaries model =
    { model
        | posX =
            Boundaries.clampX
                boundaries
                model
                (model.posX + (delta * model.xSpeed))
        , posY =
            Boundaries.clampY
                boundaries
                model
                (model.posY + (delta * model.ySpeed))
    }


view : Model -> List (Svg a)
view model =
    List.map shipView model


shipView : Ship -> Svg a
shipView { posX, posY } =
    Svg.rect
        [ width "20"
        , height "20"
        , x (toString posX)
        , y (toString posY)
        ]
        []
