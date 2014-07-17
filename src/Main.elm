module Main where

import Graphics.WebGL (..)
import Math.Matrix4 (..)
import Keyboard
import Mouse
import Window

import Model
import Engine (..)
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
    dropWhen (lift2 (&&) Keyboard.shift isLocked) () Mouse.clicks

port exitPointerLock : Signal ()
port exitPointerLock =
    always () <~ keepIf (any (\x -> x == 27)) [] Keyboard.keysDown

-- Set up 3D world
inputs : Signal Model.Inputs
inputs =
  let dt = lift (\t -> t/500) (fps 60)
      dirKeys = merge Keyboard.arrows Keyboard.wasd
  in  merge (sampleOn dt <| lift3 Model.TimeDelta Keyboard.space dirKeys dt)
            (Model.Mouse <~ movement)

person : Signal Model.Person
person = foldp Update.step Model.defaultPerson inputs

main : Signal Element
main = world Demo.demoThings

world : Signal [Thing] -> Signal Element
world entities =
  let t = foldp (+) 0 (fps 60)
      wh = Window.dimensions
  in 
      lift3 lockMessage wh isLocked
            (lift4 scene entities wh t person)

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
   plainText <|
    "This uses stuff that is only available in Chrome and Firefox!\n" ++
    "\nPress arrows or WASD keys to move, space bar to jump.\n\n" ++ msg

