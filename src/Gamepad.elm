module Gamepad
  ( gamepads
  , Button
  , Gamepad
  ) where

{-| Library for working with gamepads
# button
@docs Button
# gamepad
@docs Gamepad
# pads
@docs gamepads
-}

import Native.Gamepad
import Signal exposing (Signal)

{-| Button -}
type alias Button =
  { pressed : Bool
  , value : Float
  }

{-| Gamepad -}
type alias Gamepad =
  { id : String
  , axes : List Float
  , buttons : List Button
  , mapping : String
  -- connected
  -- index
  -- timestamp
  }

{-| gamepads -}
gamepads : Signal (List Gamepad)
gamepads =
  Native.Gamepad.gamepads

