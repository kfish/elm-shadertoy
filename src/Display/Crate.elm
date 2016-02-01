module Display.Crate (cloudsCube, fireCube, fogMountainsCube, plasmaCube, voronoiCube, cube) where

import List exposing (concatMap, map)
import Time exposing (Time, inSeconds)

import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import WebGL exposing (..)

import Shaders.Clouds exposing (clouds)
import Shaders.Fire exposing (fire)
import Shaders.FogMountains exposing (fogMountains)
import Shaders.SimplePlasma exposing (simplePlasma)
import Shaders.VoronoiDistances exposing (voronoiDistances)
import Shaders.WorldVertex exposing (Vertex, worldVertex)

import Model

type alias Triangle a = (a,a,a)

cloudsCube : (Int,Int) -> Time -> Mat4 -> Renderable
cloudsCube = cube worldVertex clouds

fireCube : (Int,Int) -> Time -> Mat4 -> Renderable
fireCube = cube worldVertex fire

fogMountainsCube : (Int,Int) -> Time -> Mat4 -> Renderable
fogMountainsCube = cube worldVertex fogMountains

plasmaCube : (Int,Int) -> Time -> Mat4 -> Renderable
plasmaCube = cube worldVertex simplePlasma

voronoiCube : (Int,Int) -> Time -> Mat4 -> Renderable
voronoiCube = cube worldVertex voronoiDistances

-- cube : Shader attributes uniforms varying -> Shader {} uniforms varyings
--    -> (Int,Int) -> Time -> Mat4 -> Renderable
cube vertexShader fragmentShader (w,h) t view =
    let resolution = vec3 (toFloat w) (toFloat h) 0
        s = inSeconds t
    in
        render vertexShader fragmentShader mesh
            { iResolution=resolution, iGlobalTime=s, view=view }

-- The mesh for a crate
-- mesh : List (Triangle Vertex)
mesh : Drawable { pos:Vec3, coord:Vec3 }
mesh = Triangle <| concatMap rotatedFace [ (0,0), (90,0), (180,0), (270,0), (0,90), (0,-90) ]

rotatedFace : (Float,Float) -> List (Triangle Vertex)
rotatedFace (angleXZ,angleYZ) =
  let x = makeRotate (degrees angleXZ) j
      y = makeRotate (degrees angleYZ) i
      t = x `mul` y
  in
      map (mapTriangle (\v -> {v | position = transform t v.position })) face

face : List (Triangle Vertex)
face =
  let topLeft     = Vertex (vec3 -1  1 1) (vec3 0 1 0)
      topRight    = Vertex (vec3  1  1 1) (vec3 1 1 0)
      bottomLeft  = Vertex (vec3 -1 -1 1) (vec3 0 0 0)
      bottomRight = Vertex (vec3  1 -1 1) (vec3 1 0 0)
  in
      [ (topLeft,topRight,bottomLeft), (bottomLeft,topRight,bottomRight) ]
