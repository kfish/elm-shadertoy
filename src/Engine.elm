module Engine where

import Math.Matrix4 (..)
import Graphics.WebGL (..)

type Perception = {
    resolution : (Int, Int),
    globalTime : Time,
    viewMatrix : Mat4
}

mapApply : [(a -> b)] -> a -> [b]
mapApply fs x = map (\f -> f x) fs

gather : [Signal (a -> b)] -> Signal (a -> [b])
gather = lift mapApply . combine

place : Float -> Float -> Float -> (Perception -> Entity) -> Perception -> Entity
place x y z obj p = obj { p | viewMatrix <- translate3 x y z p.viewMatrix }

