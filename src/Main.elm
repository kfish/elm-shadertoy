module Main (main) where

{-| This module drives a virtuul wurld

# Program entry
@docs main
-}

import Graphics.Element exposing (..)
import Set
import Signal exposing (..)
import Text
import Time exposing (Time, fps)

import WebGL exposing (..)
import Math.Matrix4 exposing (..)
import Keyboard
import Mouse
import Window

import Model
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
{-
port requestPointerLock : Signal Bool
port requestPointerLock =
    always True <~ dropWhen (lift2 (&&) Keyboard.shift isLocked) () Mouse.clicks

port exitPointerLock : Signal Bool
port exitPointerLock =
    always True <~ keepIf (any (\x -> x == 27)) [] Keyboard.keysDown
-}

-- Set up 3D world
inputs : Signal Model.Inputs
inputs =
  let dt = map (\t -> t/500) (fps 60)
      dirKeys = merge Keyboard.arrows Keyboard.wasd
  in  merge (sampleOn dt <| map3 Model.TimeDelta Keyboard.space dirKeys dt)
            (map Model.Mouse movement)

person : Signal Model.Person
person = foldp Update.step Model.defaultPerson inputs

{-| The main function -}
main : Signal Element
main = world Demo.demoThings

world : Signal (List Thing) -> Signal Element
world entities =
  let t = foldp (+) 0 (fps 60)
      wh = Window.dimensions
  in 
      Signal.map3 lockMessage wh isLocked
            (Signal.map4 scene entities wh t person)
{-
world : ((Int,Int) -> Time -> Mat4 -> List Renderable) -> Signal Element
world entities =
  let t = foldp (+) 0 (fps 60)
  -- in  map5 Display.scene (constant entities) Window.dimensions t isLocked person
  in  map5 Display.scene (constant entities) Window.dimensions t (constant True) person
-}

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
