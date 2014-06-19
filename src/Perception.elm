module Perception where

import Math.Matrix4 (..)

type Perception = {
    resolution : (Int, Int),
    globalTime : Time,
    viewMatrix : Mat4
}

