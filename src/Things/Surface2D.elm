module Things.Surface2D (surface2D) where

import Array
import Array2D exposing (Array2D)
import List exposing (..)
import List.Extra exposing (splitAt)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import Time exposing (Time, inSeconds)
import WebGL exposing (..)

import Shaders.ColorFragment exposing (colorFragment, noiseColorFragment)
import Shaders.WorldVertex exposing (Vertex, worldVertex)

import Model

surface2D = surface worldVertex noiseColorFragment << fromListsDefaults

surface vertexShader fragmentShader mesh =
    let see = seeSurface vertexShader fragmentShader mesh
    in { pos = vec3 0 0 0, orientation = vec3 1 0 1, see = see }

seeSurface vertexShader fragmentShader mesh p =
    let (w,h) = p.resolution
        resolution = vec3 (toFloat w) (toFloat h) 0
        s = inSeconds p.globalTime
    in
        [render vertexShader fragmentShader mesh
            { iResolution=resolution, iGlobalTime=s, view=p.viewMatrix }]

fromListsDefaults = surfaceMesh -256 2 2 -4 40 -256 2

----------------------------------------------------------------------

mkStrip : List Vertex -> List Vertex -> List (Vertex, Vertex, Vertex)
mkStrip vs1 vs2 = map3 (,,) vs1 vs2 (drop 1 vs1) ++ map3 (,,) vs2 (drop 1 vs1) (drop 1 vs2)

matRow : Float -> Float -> Float -> Float -> Float -> Float -> List (Float, Vec3) -> List Vertex
matRow x pos_dx coord_dx y0 ymul z =
  let m posOffset coordOffset ys0 = case ys0 of
          ((y,rgb)::ys) -> { pos = vec3 (x+posOffset) (y0+y*ymul) z, color = rgb, coord = vec3 coordOffset z 0 } :: (m (posOffset + pos_dx) (coordOffset + coord_dx) ys)
          _       -> []
  in
      m 0.0 0.0

surfaceMesh : Float -> Float -> Float -> Float -> Float -> Float -> Float
    -> List (List (Float, Vec3))
    -> Drawable Vertex
surfaceMesh x dx_pos dx_coord y0 ymul z dz m =
    let
        zs = indexedMap (\ix _ -> z + dz * toFloat ix) m
        rows = List.map2 (matRow x dx_pos dx_coord y0 ymul) zs m
    in
        Triangle <| List.concat <| List.map2 mkStrip rows (drop 1 rows)
