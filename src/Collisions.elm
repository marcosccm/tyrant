module Collisions exposing (..)


type alias Rect =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


detect : List Rect -> Rect -> Bool
detect rects target =
    List.any (collideSingle target) rects


collideSingle : Rect -> Rect -> Bool
collideSingle a b =
    (a.x < b.x + b.width)
        && (a.x + a.width > b.x)
        && (a.y < b.y + b.height)
        && (a.height + a.y > b.y)
