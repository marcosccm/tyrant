module Ship
    exposing
        ( Model
        , view
        , processInput
        , tick
        , startingShip
        , center
        )

import Svg exposing (path, Svg)
import Svg.Attributes exposing (..)
import String
import Time exposing (Time)
import PlayerActions as Action exposing (Action)


type alias Model =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    , xSpeed : Float
    , ySpeed : Float
    , xLimit : Float
    , yLimit : Float
    }


startingShip : ( Float, Float ) -> Model
startingShip ( xLimit, yLimit ) =
    { x = 10
    , y = 10
    , width = 80
    , height = 40
    , xSpeed = 0
    , ySpeed = 0
    , yLimit = yLimit
    , xLimit = xLimit
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


tick : Time -> Model -> Model
tick timeDelta model =
    { model
        | x = moveX model timeDelta
        , y = moveY model timeDelta
    }


moveY : Model -> Float -> Float
moveY model time =
    let
        delta =
            time * model.ySpeed

        next =
            model.y + delta

        limit =
            model.yLimit - model.height
    in
        Basics.max (Basics.min next limit) 0


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
