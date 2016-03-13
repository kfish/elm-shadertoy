module Main (main) where

{-| This module drives a virtuul wurld

# Program entry
@docs main
-}

import Automaton
import Char exposing (toCode)
import Graphics.Element exposing (..)
import Random
import Set
import Signal exposing (..)
import Text
import Time exposing (Time, fps)

import WebGL exposing (..)
import Math.Matrix4 exposing (..)
import Keyboard
import Mouse
import Window

import Array2D exposing (Array2D)
import Gamepad
import Math.Procedural exposing (..)
import Model exposing (noInput)
import Engine exposing (..)
import Update
import Things.Surface2D exposing (Placement, defaultPlacement)

import Demo

import Debug

-- Pointer Lock information
port movement : Signal (Int,Int)
port isLocked : Signal Bool

-- Ability to request and exit. Click screen to request lock. Press escape to
-- give up the lock. This code can all be removed if you want to do this
-- differently.

port requestPointerLock : Signal ()
port requestPointerLock =
    let dropWhen bs def sig = map snd (filter fst (False, def) (map2 (,) (sampleOn sig (map not bs)) sig)) in
    dropWhen (map2 (&&) Keyboard.shift isLocked) () Mouse.clicks


port exitPointerLock : Signal ()
port exitPointerLock =
    map (always ()) (filter (Set.member 27) Set.empty Keyboard.keysDown)

----------------------------------------------------------------------
-- Gamepad

gamepadsToArrows : List Gamepad.Gamepad -> { x : Float, y : Float, mx : Float, my : Float }
gamepadsToArrows gamepads =
    let
        sgn x = if x < 0 then -1 else 1
        -- Apply a deadzone of size l around an axis centered at 0
        u l x = if abs x < l then 0.0 else sgn x * (abs x - l) / (1.0 - l)
        deadzone = u 0.2

        -- Map a trigger to a standard range [0..1], applying a deadzone
        dc l x = let xm = (x+1.0)/2.0 in u l xm
        toTrigger = dc 0.2

        axs = case List.head gamepads of
            Nothing -> {x=0, y=0, mx=0, my=0 }
            Just gamepad ->
              case gamepad.axes of
                  -- Performance Designed Products Rock Candy Gamepad for Xbox 360 (Vendor: 0e6f Product: 011f)
                  [x1, y1, b1, x2, y2, b2, x3, y3] ->
                      { x = deadzone x1 / 10
                      , y = deadzone (-1.0 * y1) / 10 - toTrigger b1 + toTrigger b2
                      , mx = deadzone x2 /20
                      , my = deadzone (-1.0 * y2) / 20
                      }

                  -- ©Microsoft Corporation Controller (STANDARD GAMEPAD Vendor: 045e Product: 028e)
                  [x1, y1, x2, y2] ->
                      { x = deadzone x1 / 10
                      , y = deadzone (-1.0 * y1) / 10
                      , mx= deadzone x2 / 20
                      , my= deadzone (-1.0 * y2) / 20
                      }

                  [x1,y1] ->
                      { x = x1/10, y = (-1.0 * y1)/10, mx = 0, my = 0 }

                  _ ->
                      {x = 0, y = 0, mx = 0, my = 0 }

        -- Interpret brake, accelerator as y input
        btns_y = case List.head gamepads of
            Nothing -> 0
            Just gamepad ->
                case gamepad.buttons of
                    -- ©Microsoft Corporation Controller (STANDARD GAMEPAD Vendor: 045e Product: 028e)
                    [a, b, x, y, lt, rt, l, r, back, start, lstick, rstick, padU, padD, padL, padR, logo] -> r.value - l.value

                    -- Performance Designed Products Rock Candy Gamepad for Xbox 360 (Vendor: 0e6f Product: 011f)
                    [a, b, x, y, lt, rt, back, start, logo, lstick, rstick] -> 0

                    _ -> 0

    in
        { axs | y = btns_y + axs.y }

gamepadNothing : Gamepad.Gamepad
gamepadNothing = { id = "", axes = [], buttons = [], mapping = "" }

-- Only set each button .pressed on a rising edge, ie. when .pressed transitions from False to True.
-- Preserves button .value
gamepadEdge : Gamepad.Gamepad -> Gamepad.Gamepad -> Gamepad.Gamepad
gamepadEdge prev now =
    let d p n = if (n.pressed && not p.pressed) then n else { n | pressed = False }
        newlyPressed = List.map2 d prev.buttons now.buttons
    in { now | buttons = newlyPressed }

gamepadsPressed : Signal (List Gamepad.Gamepad)
gamepadsPressed =
    let
        -- Like List.map2, but ensures that there are always as many
        -- elements in the result as in the right list
        map2Now f now prev =
            case (prev, now) of
                (_      , []     ) -> []
                ([]     , _      ) -> now
                ((p::ps), (n::ns)) -> (f p n :: map2Now f ps ns)
    in
        foldp (map2Now gamepadEdge) [] Gamepad.gamepads

{- a b x y ltop rtop l r 
l tiny (back), r tiny (start)
stick l down, stick r down
4way: u d l r
Xbox-logo
-}

type alias GamepadButtons = { bA : Bool, bB : Bool, bX : Bool, bY : Bool, bBack : Bool, bStart : Bool }

gamepadButtonsNone = { bA = False, bB = False, bX = False, bY = False, bBack = False, bStart = False }

gamepadsToButtons : List Gamepad.Gamepad -> GamepadButtons
gamepadsToButtons gamepads =
    case List.head gamepads of
        Nothing -> gamepadButtonsNone
        Just gamepad ->
            case gamepad.buttons of
                (a::b::x::y::lt::rt::l::r::back::start::_) ->
                    { bA = a.pressed, bB = b.pressed, bX = x.pressed, bY = y.pressed,
                      bBack = back.pressed, bStart = start.pressed }

                _ -> gamepadButtonsNone

----------------------------------------------------------------------

gamepadsToInputs : List Gamepad.Gamepad -> Time -> Model.Inputs
gamepadsToInputs gamepads dt =
    let {x,y,mx,my} = gamepadsToArrows gamepads
        {bA, bB, bX, bY, bBack, bStart} = gamepadsToButtons gamepads
    in  { noInput | reset = bStart, x = x, y = y, mx=mx, my=my, button_X = bX, dt = dt }

mouseDeltas : Signal (Time, (Float, Float))
mouseDeltas =
    let step (t,(mx,my)) pt = ((Time.inSeconds t - pt, (toFloat mx, toFloat my)), t)
        a = Automaton.hiddenState 0 step
    in Automaton.run a (0, (0,0)) (Time.timestamp movement)

-- Set up 3D world
kbMouseInputs : Signal Model.Inputs
kbMouseInputs =
  let dt = map Time.inSeconds (fps 60)
      dirKeys = merge Keyboard.arrows Keyboard.wasd
      yo x = x / 500
      handleKeys s bx {x,y} kdt = dbg_X
          { noInput | isJumping = s, button_X = bx, x = toFloat x, y = toFloat y, dt=kdt }
      dbg_X i = if i.button_X then Debug.log "Button X pressed" i else i
      x_key = Keyboard.isDown 88
  in  merge
          (sampleOn dt <| map4 handleKeys Keyboard.space x_key dirKeys dt)
          (map (\(mt, (mx,my)) -> { noInput | mx= yo mx, my= yo my, mt = mt }) mouseDeltas)

gamepadInputs : Signal Model.Inputs
gamepadInputs =
  let dt = map Time.inSeconds (fps 60)
  in  sampleOn dt <| map2 gamepadsToInputs gamepadsPressed dt

inputs : Signal Model.Inputs
inputs = merge (dropRepeats kbMouseInputs) (dropRepeats gamepadInputs)

person : Placement -> Array2D Float -> Signal Model.Person
person placement terrain = foldp (Update.step placement terrain) Model.defaultPerson inputs

{-| The main function -}
main : Signal Element
main = world Demo.demoThings

world : (Array2D Float -> Signal (List Thing)) -> Signal Element
world thingsOnTerrain =
  let t = foldp (+) 0 (fps 30)
      wh = Window.dimensions

      placement = defaultPlacement

      seed0 = Random.initialSeed 7777
      (terrain, seed1) = Random.generate (randTerrain2D (placement.bigSide+1)) seed0
      entities = thingsOnTerrain terrain
  in 
      Signal.map3 lockMessage wh isLocked
            (Signal.map4 scene entities wh t (person placement terrain))

lockMessage : (Int,Int) -> Bool -> Element -> Element
lockMessage (w,h) isLocked e =
    layers [ e
           , container w 140 (midLeftAt (absolute 40) (relative 0.5))
                 (if isLocked then exitMsg else enterMsg)
           ]

enterMsg : Element
enterMsg = message "Click to go full screen and move your head with the mouse."

exitMsg : Element
exitMsg = message "Press <escape> to exit full screen."

message : String -> Element
message msg =
   leftAligned <| Text.monospace <| Text.fromString <|
    "Use gamepad, arrows or WASD keys to move.\n\n" ++ msg
