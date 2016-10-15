module Boundaries exposing (..)


type alias Boundaries =
    { x : Float
    , y : Float
    }


init : Float -> Float -> Boundaries
init x y =
    { x = x, y = y }


clampX : Boundaries -> { a | width : Float } -> Float -> Float
clampX { x } { width } attempt =
    Basics.max (Basics.min attempt x) 0


clampY : Boundaries -> { a | height : Float } -> Float -> Float
clampY { y } { height } attempt =
    Basics.max (Basics.min attempt (y - height)) 0
