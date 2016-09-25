module Tyrant exposing (..)

import Html.App as App
import Game


main =
    App.program
        { init = ( Game.init, Cmd.none )
        , view = Game.view
        , update = Game.update
        , subscriptions = Game.subscriptions
        }
