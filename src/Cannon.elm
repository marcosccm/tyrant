module Cannon exposing (Model, init, processInput, tick, view)

import Time exposing (Time)
import PlayerActions as Action exposing (Action)
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Bullet =
    { x : Float
    , y : Float
    , height : Float
    , width : Float
    , xSpeed : Float
    }


type alias Model =
    { shots : List Bullet }


init : Model
init =
    { shots = [] }


newShot : ( Float, Float ) -> Bullet
newShot ( x, y ) =
    { x = x
    , y = y
    , width = 5
    , height = 5
    , xSpeed = 1
    }


tick : Time -> Model -> Model
tick timeDelta model =
    let
        updatedShots =
            List.map (tickBullet timeDelta) model.shots
    in
        { model | shots = updatedShots }


tickBullet : Time -> Bullet -> Bullet
tickBullet timeDelta bullet =
    { bullet | x = moveX bullet timeDelta }


moveX : Bullet -> Float -> Float
moveX model time =
    let
        delta =
            time * model.xSpeed

        next =
            model.x + delta
    in
        Basics.max next 0


processInput : Action -> Model -> ( Float, Float ) -> Model
processInput action model position =
    case action of
        Action.Shoot ->
            { model | shots = (newShot position) :: model.shots }

        _ ->
            model


view : Model -> List (Svg a)
view model =
    List.map renderBullet model.shots


renderBullet : Bullet -> Svg a
renderBullet bullet =
    rect
        [ x (toString bullet.x)
        , y (toString bullet.y)
        , width (toString bullet.width)
        , height (toString bullet.height)
        ]
        []
