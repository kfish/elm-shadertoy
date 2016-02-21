module Things.Ground (ground) where

import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import Util exposing (hslToVec3)
import WebGL exposing (..)

import Shaders.ColorFragment exposing (colorFragment)
-- import Shaders.ColorVertex exposing (ColorVertex, colorVertex)
import Shaders.WorldVertex exposing (Vertex, worldVertex)
import Engine exposing (..)

ground : Signal Thing
ground = Signal.constant <| Thing (vec3 0 0 0) (vec3 1 0 1) seeGround

seeGround p =
    [render worldVertex colorFragment groundMesh { view=p.viewMatrix }]


-- The mesh for the ground
groundMesh : Drawable Vertex
groundMesh =
  let green = hslToVec3 (degrees 110) 0.48

      topLeft     = { pos = vec3 -20 -1  20, color = green 0.7, coord = vec3 0 0 0 }
      topRight    = { pos = vec3  20 -1  20, color = green 0.4, coord = vec3 0 0 0 }
      bottomLeft  = { pos = vec3 -20 -1 -20, color = green 0.5, coord = vec3 0 0 0 }
      bottomRight = { pos = vec3  20 -1 -20, color = green 0.6, coord = vec3 0 0 0 }
  in
      Triangle [ (topLeft,topRight,bottomLeft), (bottomLeft,topRight,bottomRight) ]
