module Laser
    exposing
        ( Model
        , Msg
        , view
        , update
        , subscriptions
        , shoot
        )

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time)
import AnimationFrame


type alias Model =
    { x : Float
    , y : Float
    , height : Float
    , width : Float
    , xSpeed : Float
    , xLimit : Float
    }


shoot : ( Float, Float ) -> Float -> Model
shoot ( x, y ) xLimit =
    { x = x
    , y = y
    , width = 5
    , height = 5
    , xSpeed = 1
    , xLimit = xLimit
    }


type Msg
    = Tick Float


update : Msg -> Model -> Model
update message model =
    case message of
        Tick timeDelta ->
            { model | x = moveX model timeDelta }


moveX : Model -> Float -> Float
moveX model time =
    let
        delta =
            time * model.xSpeed

        next =
            model.x + delta

        limit =
            model.xLimit - model.width
    in
        Basics.max (Basics.min next limit) 0


subscriptions : Sub Msg
subscriptions =
    AnimationFrame.diffs Tick


view : Model -> Svg a
view model =
    rect
        [ x (toString model.x)
        , y (toString model.y)
        , width (toString model.width)
        , height (toString model.height)
        ]
        []
