module Main exposing (main)

{-| This module drives a virtuul wurld

# Program entry
@docs main
-}

import AnimationFrame
import Html.App as Html
import Keyboard
import Mouse
import Window

import Ports

import Model
import Update
import View

{-
-- import Graphics.Element exposing (Element)
import Set
-- import Signal exposing (..)
import Time exposing (Time, fps)

import WebGL exposing (..)
import Math.Matrix4 exposing (..)
import Keyboard
import Mouse
import Window

import Model
import Update
import Display
-}

{-| The main entrypoint -}
main : Program Model.Args
main =
    Html.programWithFlags
        { init = Model.init
        , update = Update.update
        , subscriptions = subscriptions
        , view = View.view
        }

{- Subscribe to keychange events.

Ignore anything that isn't an escape, space or WASD keys.
-}
keyChange : Bool -> Keyboard.KeyCode -> Model.Msg
keyChange on keyCode =
    if keyCode == 27 && on then
        Model.LockRequest False
    else
        (case keyCode of
            32 -> \k -> { k | space = on }
            65 -> \k -> { k | left  = on }
            68 -> \k -> { k | right = on }
            87 -> \k -> { k | up    = on }
            83 -> \k -> { k | down  = on }
            _  -> Basics.identity
        ) |> Model.KeyChange

subscriptions : Model.Model -> Sub Model.Msg
subscriptions model =
    [ AnimationFrame.diffs Model.Animate
    , Keyboard.downs (keyChange True)
    , Keyboard.ups (keyChange False)
    , Window.resizes Model.Resize
    , Ports.isLocked Model.LockUpdate
    ]
        ++ (if model.isLocked then
                [ Ports.movement Model.MouseMove ]
            else
                [ Mouse.clicks (\_ -> Model.LockRequest True) ]
           )
        |> Sub.batch

{-
-- main = world Display.crateEntities

-- Pointer Lock information
port movement : Signal (Int,Int)
port isLocked : Signal Bool

-- Set up 3D world
inputs : Signal Model.Inputs
inputs =
  let dt = map (\t -> t/500) (fps 60)
      dirKeys = merge Keyboard.arrows Keyboard.wasd
  in  merge (sampleOn dt <| map3 Model.TimeDelta Keyboard.space dirKeys dt)
            (map Model.Mouse movement)

person : Signal Model.Person
person = foldp Update.step Model.defaultPerson inputs

world : ((Int,Int) -> Time -> Mat4 -> List Renderable) -> Signal Element
world entities =
  let t = foldp (+) 0 (fps 60)
  -- in  map5 Display.scene (constant entities) Window.dimensions t isLocked person
  in  map5 Display.scene (constant entities) Window.dimensions t (constant True) person

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
-}
