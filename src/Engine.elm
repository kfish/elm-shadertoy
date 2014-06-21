module Engine where

import Math.Vector3 (..)
import Math.Matrix4 (..)
import Graphics.WebGL (..)

import Model

type Perception = {
    resolution : (Int, Int),
    globalTime : Time,
    viewMatrix : Mat4
}

type Thing = (Perception -> Entity)
type Things = (Perception -> [Entity])

mapApply : [(a -> b)] -> a -> [b]
mapApply fs x = map (\f -> f x) fs

gather : [Signal (a -> b)] -> Signal (a -> [b])
gather = lift mapApply . combine

place : Float -> Float -> Float -> (Perception -> Entity) -> Perception -> Entity
place x y z obj p = obj { p | viewMatrix <- translate3 x y z p.viewMatrix }

look : (Int,Int) -> Model.Person -> Mat4
look (w,h) person =
    mul (makePerspective 45 (toFloat w / toFloat h) 0.01 100)
        (makeLookAt person.position (person.position `add` Model.direction person) j)

scene : (Perception -> [Entity])
    -> (Int,Int) -> Time -> Model.Person -> Element
scene entities (w,h) t person =
  let
    p = { viewMatrix = look (w,h) person, globalTime = t, resolution = (w,h) }
  in
    layers [ color (rgb 135 206 235) (spacer w h)
           , webgl (w,h) (entities p)
           ]

