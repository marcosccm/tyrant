module PlayerActions exposing (Action(..), subscriptions)

import Keyboard exposing (KeyCode)


type Action
    = MoveLeft
    | MoveRight
    | MoveUp
    | MoveDown
    | StopLateralMovement
    | StopUpwardsMovement
    | Shoot
    | NoOp


keyUp : KeyCode -> Action
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


keyDown : KeyCode -> Action
keyDown keyCode =
    case keyCode of
        32 ->
            Shoot

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


subscriptions : Sub Action
subscriptions =
    Sub.batch
        [ Keyboard.downs keyDown
        , Keyboard.ups keyUp
        ]
