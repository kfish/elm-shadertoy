module Things.Cube (cloudsCube, fireCube, fogMountainsCube, plasmaCube, voronoiCube, xvCube, cube) where

import Math.Vector2 (Vec2)
import Math.Vector3 (..)
import Math.Matrix4 (..)
import Graphics.WebGL (..)

import Shaders.Clouds (clouds)
import Shaders.Fire (fire)
import Shaders.FogMountains (fogMountains)
import Shaders.SimplePlasma (simplePlasma)
import Shaders.VoronoiDistances (voronoiDistances)
import Shaders.WorldVertex (Vertex, worldVertex)

import Model
import Engine (..)

import Char (..)
import Keyboard (isDown)

-- cloudsCube : Signal Thing
cloudsCube = constant <| cube worldVertex clouds

-- fireCube : Signal Thing
fireCube = constant <| cube worldVertex fire

-- fogMountainsCube : Signal Thing
fogMountainsCube = constant <| cube worldVertex fogMountains

-- plasmaCube : Signal Thing
plasmaCube = constant <| cube worldVertex simplePlasma

-- voronoiCube : Signal Thing
voronoiCube = constant <| cube worldVertex voronoiDistances

-- xvCube : Signal (Bool, Thing)
xvCube = lift2 (,) (isDown (toCode 'x')) voronoiCube

-- cube : Oriented (Visible {})
cube vertexShader fragmentShader =
    let see = seeCube vertexShader fragmentShader
    in { position = vec3 0 0 0, orientation = vec3 1 0 1, see = see }

-- cube : Shader attributes uniforms varying -> Shader {} uniforms varyings
--    -> Perception -> Entity
seeCube vertexShader fragmentShader p =
    let (w,h) = p.resolution
        resolution = vec3 (toFloat w) (toFloat h) 0
        s = inSeconds p.globalTime
    in
        [entity vertexShader fragmentShader mesh
            { iResolution=resolution, iGlobalTime=s, view=p.viewMatrix }]

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
