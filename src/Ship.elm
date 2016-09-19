module Ship
    exposing
        ( Model
        , Msg
        , view
        , update
        , subscriptions
        , startingShip
        )

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time)
import Keyboard exposing (KeyCode)
import AnimationFrame


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
    , width = 60
    , height = 50
    , xSpeed = 0
    , ySpeed = 0
    , yLimit = yLimit
    , xLimit = xLimit
    }


type Msg
    = Tick Float
    | MoveLeft
    | MoveRight
    | MoveUp
    | MoveDown
    | StopLateralMovement
    | StopUpwardsMovement
    | NoOp


update : Msg -> Model -> Model
update message model =
    case message of
        MoveLeft ->
            { model | xSpeed = -0.4 }

        MoveRight ->
            { model | xSpeed = 0.4 }

        MoveUp ->
            { model | ySpeed = -0.4 }

        MoveDown ->
            { model | ySpeed = 0.4 }

        StopLateralMovement ->
            { model | xSpeed = 0 }

        StopUpwardsMovement ->
            { model | ySpeed = 0 }

        Tick timeDelta ->
            { model
                | x = moveX model timeDelta
                , y = moveY model timeDelta
            }

        NoOp ->
            model


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


keyUp : KeyCode -> Msg
keyUp keyCode =
    case keyCode of
        37 ->
            StopLateralMovement

        39 ->
            StopLateralMovement

        38 ->
            StopUpwardsMovement

        40 ->
            StopUpwardsMovement

        _ ->
            NoOp


keyDown : KeyCode -> Msg
keyDown keyCode =
    case keyCode of
        37 ->
            MoveLeft

        39 ->
            MoveRight

        38 ->
            MoveUp

        40 ->
            MoveDown

        _ ->
            NoOp


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Keyboard.downs keyDown
        , Keyboard.ups keyUp
        ]


view : Model -> Svg a
view model =
    rect
        [ x (toString model.x)
        , y (toString model.y)
        , width (toString model.width)
        , height (toString model.height)
        ]
        []
