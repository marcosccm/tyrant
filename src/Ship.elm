module Ship
    exposing
        ( Model
        , view
        , processInput
        , tick
        , init
        , center
        )

import Svg exposing (path, Svg)
import Svg.Attributes exposing (..)
import String
import Time exposing (Time)
import PlayerActions as Action exposing (Action)
import Boundaries exposing (Boundaries)


type alias Model =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    , xSpeed : Float
    , ySpeed : Float
    }


init : Model
init =
    { x = 10
    , y = 200
    , width = 80
    , height = 40
    , xSpeed = 0
    , ySpeed = 0
    }


center : Model -> ( Float, Float )
center { x, y, width, height } =
    ( x + width / 2, y + height / 2 )


processInput : Action -> Model -> Model
processInput action model =
    case action of
        Action.MoveLeft ->
            { model | xSpeed = -0.4 }

        Action.MoveRight ->
            { model | xSpeed = 0.4 }

        Action.MoveUp ->
            { model | ySpeed = -0.4 }

        Action.MoveDown ->
            { model | ySpeed = 0.4 }

        Action.StopLateralMovement ->
            { model | xSpeed = 0 }

        Action.StopUpwardsMovement ->
            { model | ySpeed = 0 }

        _ ->
            model


tick : Time -> Boundaries -> Model -> Model
tick delta boundaries model =
    { model
        | x =
            Boundaries.clampX
                boundaries
                model
                (model.x + (delta * model.xSpeed))
        , y =
            Boundaries.clampY
                boundaries
                model
                (model.y + (delta * model.ySpeed))
    }


view : Model -> Svg a
view model =
    Svg.path
        [ d (calculatePath model)
        , stroke "black"
        ]
        []


calculatePath : Model -> String
calculatePath { x, y, height, width } =
    String.join " " <|
        [ component "M" [ x, y ]
        , component "v" [ height ]
        , component "l" [ width, (-height / 2) ]
        , "Z"
        ]


component : String -> List Float -> String
component command values =
    values
        |> List.map toString
        |> (::) command
        |> String.join " "
