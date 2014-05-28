module Display.Crate (fireCube, plasmaCube, voronoiCube, cube) where

import Math.Vector2 (Vec2)
import Math.Vector3 (..)
import Math.Matrix4 (..)
import Graphics.WebGL (..)

import Shaders.Fire (fire)
-- import Shaders.FogMountains (fogMountains)
import Shaders.SimplePlasma (simplePlasma)
import Shaders.VoronoiDistances (voronoiDistances)
import Shaders.WorldVertex (Vertex, worldVertex)

import Model

fireCube : (Int,Int) -> Time -> Mat4 -> Entity
fireCube = cube worldVertex fire

plasmaCube : (Int,Int) -> Time -> Mat4 -> Entity
plasmaCube = cube worldVertex simplePlasma

voronoiCube : (Int,Int) -> Time -> Mat4 -> Entity
voronoiCube = cube worldVertex voronoiDistances

-- cube : Shader attributes uniforms varying -> Shader {} uniforms varyings
--    -> (Int,Int) -> Time -> Mat4 -> Entity
cube vertexShader fragmentShader (w,h) t view =
    let resolution = vec3 (toFloat w) (toFloat h) 0
        s = inSeconds t
    in
        entity vertexShader fragmentShader mesh
            { iResolution=resolution, iGlobalTime=s, view=view }

-- The mesh for a crate
mesh : [Triangle Vertex]
mesh = concatMap rotatedFace [ (0,0), (90,0), (180,0), (270,0), (0,90), (0,-90) ]

rotatedFace : (Float,Float) -> [Triangle Vertex]
rotatedFace (angleXZ,angleYZ) =
  let x = makeRotate (degrees angleXZ) j
      y = makeRotate (degrees angleYZ) i
      t = x `mul` y
  in
      map (mapTriangle (\v -> {v | position <- transform t v.position })) face

face : [Triangle Vertex]
face =
  let topLeft     = Vertex (vec3 -1  1 1) (vec3 0 1 0)
      topRight    = Vertex (vec3  1  1 1) (vec3 1 1 0)
      bottomLeft  = Vertex (vec3 -1 -1 1) (vec3 0 0 0)
      bottomRight = Vertex (vec3  1 -1 1) (vec3 1 0 0)
  in
      [ (topLeft,topRight,bottomLeft), (bottomLeft,topRight,bottomRight) ]
