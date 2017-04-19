module View.Crate exposing (textureCube, cloudsCube, fireCube, fogMountainsCube, plasmaCube, voronoiCube, cube)

import Time exposing (Time, inSeconds)

import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import WebGL exposing (..)
import Window

import Shaders.Clouds exposing (clouds)
import Shaders.Fire exposing (fire)
import Shaders.FogMountains exposing (fogMountains)
import Shaders.SimplePlasma exposing (simplePlasma)
import Shaders.VoronoiDistances exposing (voronoiDistances)
import Shaders.TextureFragment exposing (textureFragment)
import Shaders.WorldVertex exposing (Vertex, worldVertex)

cloudsCube : Window.Size -> Time -> Mat4 -> Entity
cloudsCube = cube worldVertex clouds

fireCube : Window.Size -> Time -> Mat4 -> Entity
fireCube = cube worldVertex fire

fogMountainsCube : Window.Size -> Time -> Mat4 -> Entity
fogMountainsCube = cube worldVertex fogMountains

plasmaCube : Window.Size -> Time -> Mat4 -> Entity
plasmaCube = cube worldVertex simplePlasma

voronoiCube : Window.Size -> Time -> Mat4 -> Entity
voronoiCube = cube worldVertex voronoiDistances

-- cube : Shader attributes uniforms varying -> Shader {} uniforms varyings
--    -> (Int,Int) -> Time -> Mat4 -> Entity
cube vertexShader fragmentShader windowSize t view =
    let resolution = vec3 (toFloat windowSize.width) (toFloat windowSize.height) 0
        s = inSeconds t
    in
        entity vertexShader fragmentShader mesh
            { iResolution=resolution, iGlobalTime=s, view=view }

textureCube : WebGL.Texture -> Mat4 -> Entity
textureCube texture perspective =
    WebGL.entity worldVertex textureFragment mesh { iTexture = texture, view = perspective }

{-| The mesh for a cube -}
mesh : Mesh { pos:Vec3, coord:Vec3 }
mesh = triangles <| List.concatMap rotatedFace [ (0,0), (90,0), (180,0), (270,0), (0,90), (0,-90) ]

rotatedFace : (Float,Float) -> List ({ pos:Vec3, coord:Vec3 }, { pos:Vec3, coord:Vec3 }, { pos:Vec3, coord:Vec3 })
rotatedFace (angleX,angleY) =
  let
    x = makeRotate (degrees angleX) (vec3 1 0 0)
    y = makeRotate (degrees angleY) (vec3 0 1 0)
    t = mul x (mul y (makeTranslate (vec3 0 0 1)))
    each f (a,b,c) =
      (f a, f b, f c)
  in
    List.map (each (\x -> {x | pos = transform t x.pos })) face


face : List ({ pos:Vec3, coord:Vec3 }, { pos:Vec3, coord:Vec3 }, { pos:Vec3, coord:Vec3 })
face =
  let
    topLeft     = { pos = vec3 -1  1 0, coord = vec3 0 1 0 }
    topRight    = { pos = vec3  1  1 0, coord = vec3 1 1 0 }
    bottomLeft  = { pos = vec3 -1 -1 0, coord = vec3 0 0 0 }
    bottomRight = { pos = vec3  1 -1 0, coord = vec3 1 0 0 }
  in
    [ (topLeft,topRight,bottomLeft)
    , (bottomLeft,topRight,bottomRight)
    ]
