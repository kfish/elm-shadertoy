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
        u l x = if abs x < l then 0.0 else (x - l) / (1.0 - l)
        use = u 0.2
        sens {x,y,mx,my} = {x=(use x)/10, y=(use y)/10, mx=(use mx)/20, my=(use my)/20 }

        axs = case List.head gamepads of
            Nothing -> {x=0, y=0, mx=0, my=0 }
            Just gamepad ->
              case gamepad.axes of
                  (x1 :: y1 :: x2 :: y2 :: _) ->
                      sens { x= x1, y= (-1.0 * y1), mx= x2, my= (-1.0 * y2) }

                  [x1,y1] ->
                      sens { x = x1, y = (-1.0 * y1), mx = 0, my = 0 }

                  _ ->
                      {x = 0, y = 0, mx = 0, my = 0 }

        -- Interpret brake, accelerator as y input
        btns_y = case List.head gamepads of
            Nothing -> 0
            Just gamepad ->
                case gamepad.buttons of
                    (a::b::x::y::lt::rt::l::r::_) -> r.value - l.value
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

person : Array2D Float -> Signal Model.Person
person terrain = foldp (Update.step terrain) Model.defaultPerson inputs

{-| The main function -}
main : Signal Element
main = world Demo.demoThings

world : (Array2D Float -> Signal (List Thing)) -> Signal Element
world thingsOnTerrain =
  let t = foldp (+) 0 (fps 30)
      wh = Window.dimensions

      seed0 = Random.initialSeed 7777
      (terrain, seed1) = Random.generate (randTerrain2D 1025) seed0
      entities = thingsOnTerrain terrain
  in 
      Signal.map3 lockMessage wh isLocked
            (Signal.map4 scene entities wh t (person terrain))

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
