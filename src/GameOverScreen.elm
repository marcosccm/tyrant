module GameOverScreen exposing (view)

import Svg exposing (Svg, text', text)
import Svg.Attributes as Attr
import Boundaries exposing (Boundaries)


view : Boundaries -> Svg a
view { x, y } =
    Svg.text'
        [ Attr.x (x / 2 |> toString)
        , Attr.y (y / 2 |> toString)
        , Attr.color "black"
        ]
        [ text "GAME OVER" ]
