module View.Crate exposing (renderCrate)

{-
module View.Crate exposing (cloudsCube, fireCube, fogMountainsCube, plasmaCube, voronoiCube, cube)

import List exposing (concatMap, map)
import Time exposing (Time, inSeconds)
-}

import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import WebGL exposing (..)

{-
import Shaders.Clouds exposing (clouds)
import Shaders.Fire exposing (fire)
import Shaders.FogMountains exposing (fogMountains)
import Shaders.SimplePlasma exposing (simplePlasma)
import Shaders.VoronoiDistances exposing (voronoiDistances)
-}
import Shaders.TextureFragment exposing (textureFragment)
import Shaders.WorldVertex exposing (Vertex, worldVertex)

{-
import Model

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
-}

{-| Render the visible renderCrate
-}
renderCrate : WebGL.Texture -> Mat4 -> WebGL.Renderable
renderCrate texture perspective =
    WebGL.render worldVertex textureFragment mesh { iTexture = texture, view = perspective }

{-| The mesh for a cube -}
mesh : Drawable { pos:Vec3, coord:Vec3 }
mesh = Triangle <| List.concatMap rotatedFace [ (0,0), (90,0), (180,0), (270,0), (0,90), (0,-90) ]

rotatedFace : (Float,Float) -> List ({ pos:Vec3, coord:Vec3 }, { pos:Vec3, coord:Vec3 }, { pos:Vec3, coord:Vec3 })
rotatedFace (angleX,angleY) =
  let
    x = makeRotate (degrees angleX) (vec3 1 0 0)
    y = makeRotate (degrees angleY) (vec3 0 1 0)
    t = x `mul` y `mul` makeTranslate (vec3 0 0 1)
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
