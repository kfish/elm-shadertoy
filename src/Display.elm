module Display (scene) where

import Http (..)
import Math.Vector2 (Vec2)
import Math.Vector3 (..)
import Math.Matrix4 (..)
import Graphics.WebGL (..)

import Model
import Engine (..)

view : (Int,Int) -> Model.Person -> Mat4
view (w,h) person =
    mul (makePerspective 45 (toFloat w / toFloat h) 0.01 100)
        (makeLookAt person.position (person.position `add` Model.direction person) j)

scene : (Perception -> [Entity])
    -> (Int,Int) -> Time -> Model.Person -> Element
scene entities (w,h) t person =
  let
    p = { viewMatrix = view (w,h) person, globalTime = t, resolution = (w,h) }
  in
    layers [ color (rgb 135 206 235) (spacer w h)
           , webgl (w,h) (entities p)
           ]

