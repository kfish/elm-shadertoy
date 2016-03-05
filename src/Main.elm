module Main (main) where

{-| This module drives a virtuul wurld

# Program entry
@docs main
-}

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

-- TODO: make a Signal Input based off gamepad controls, emulating both
-- keyboard arrows and mouse movement.
-- ... THEN, rename those constructors so they are not "Keyboard" and "Mouse"
-- ... THEN, allow float types for the movement controls
-- ... THEN, ... use other buttons for acceleration, braking etc.

gamepadsToArrows : List Gamepad.Gamepad -> { x : Float, y : Float, mx : Float, my : Float }
gamepadsToArrows gamepads =
    let
        u l x = if abs x < l then 0.0 else (x - l) / (1.0 - l)
        use = u 0.2
        sens {x,y,mx,my} = {x=(use x)/10, y=(use y)/10, mx=(use mx)/20, my=(use my)/20}

        axs = case List.head gamepads of
            Nothing -> {x=0, y=0, mx=0, my=0}
            Just gamepad ->
              case gamepad.axes of
                  (x1 :: y1 :: x2 :: y2 :: _) ->
                      sens { x= x1, y= (-1.0 * y1), mx= x2, my= (-1.0 * y2) }

                  [x1,y1] ->
                      sens { x = x1, y = (-1.0 * y1), mx = 0, my = 0 }

                  _ ->
                      {x = 0, y = 0, mx = 0, my = 0 }

        btns = case List.head gamepads of
            Nothing -> 0
            Just gamepad ->
                case gamepad.buttons of
                    (_::_::_::_::_::_::l::r::_) -> r.value - l.value
                    _ -> 0

    in
        { axs | y = btns + axs.y }

gamepadsToInputs : List Gamepad.Gamepad -> Time -> Model.Inputs
gamepadsToInputs gamepads dt =
    let {x,y,mx,my} = gamepadsToArrows gamepads
    in  { noInput | x = x, y = y, mx=mx, my=my, dt = dt }

-- Set up 3D world
kbMouseInputs : Signal Model.Inputs
kbMouseInputs =
  let dt = map (\t -> t/500) (fps 60)
      dirKeys = merge Keyboard.arrows Keyboard.wasd
      yo x = toFloat x / 500
  in  merge (sampleOn dt <| map3 (\s {x,y} kdt -> { noInput | isJumping = s, x = toFloat x, y = toFloat y, dt=kdt }) Keyboard.space dirKeys dt)
            (map (\(mdt, (mx,my)) -> { noInput | mx= yo mx, my= yo my, dt=mdt }) (Time.timestamp movement))

gamepadInputs : Signal Model.Inputs
gamepadInputs =
  let dt = map (\t -> t/500) (fps 60)
  in  sampleOn dt <| map2 gamepadsToInputs Gamepad.gamepads dt

inputs : Signal Model.Inputs
inputs = merge kbMouseInputs gamepadInputs

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
    "This uses stuff that is only available in Chrome and Firefox!\n" ++
    "\nPress arrows or WASD keys to move, space bar to jump.\n\n" ++ msg
