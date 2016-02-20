module Things.Ground (ground) where

import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import Util exposing (hsvToVec3)
import WebGL exposing (..)

import Shaders.ColorFragment exposing (colorFragment)
import Shaders.ColorVertex exposing (ColorVertex, colorVertex)
import Engine exposing (..)

ground : Signal Thing
ground = Signal.constant <| Thing (vec3 0 0 0) (vec3 1 0 1) seeGround

seeGround p =
    [render colorVertex colorFragment groundMesh { view=p.viewMatrix }]


-- The mesh for the ground
groundMesh : Drawable ColorVertex
groundMesh =
  let green = hsvToVec3 (degrees 110) 0.48

      topLeft     = ColorVertex (vec3 -20 -1  20) (green 0.7)
      topRight    = ColorVertex (vec3  20 -1  20) (green 0.4)
      bottomLeft  = ColorVertex (vec3 -20 -1 -20) (green 0.5)
      bottomRight = ColorVertex (vec3  20 -1 -20) (green 0.6)
  in
      Triangle [ (topLeft,topRight,bottomLeft), (bottomLeft,topRight,bottomRight) ]
