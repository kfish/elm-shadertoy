module Things.Surface2D (testSurface, testSurfaceArr) where

import Array
import Array2D exposing (Array2D)
import List exposing (..)
import List.Extra exposing (splitAt)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import Time exposing (Time, inSeconds)
import WebGL exposing (..)

import Shaders.Clouds exposing (clouds)
import Shaders.Fire exposing (fire)
import Shaders.FogMountains exposing (fogMountains)
--import Shaders.SimplePlasma exposing (simplePlasma)
import Shaders.VoronoiDistances exposing (voronoiDistances)
import Shaders.WorldVertex exposing (Vertex, worldVertex)

import Model

testSurface = Signal.constant <| surface worldVertex clouds testSurfaceMesh

testSurfaceArr arr0 = surface worldVertex fire <| testSurfaceMeshArr arr0

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

testSurfaceMesh = surfaceMesh 0 1.4 1.1 10 0 1
    [ [1.0, 0.8, 0.7, 0.4, 0.3, 0.1, 0.2, 0.5, 0.6, 1.0]
    , [0.9, 0.75, 0.63, 0.3, 0.21, 0.2, 0.23, 0.4, 0.56, 0.8]
    , [0.8, 0.65, 0.53, 0.2, 0.11, 0.23, 0.18, 0.3, 0.46, 0.6]
    ]

testSurfaceMeshArr = surfaceMeshArr 0 2 2 70 0 2

----------------------------------------------------------------------

mkStrip : List Vertex -> List Vertex -> List (Vertex, Vertex, Vertex)
mkStrip vs1 vs2 = map3 (,,) vs1 vs2 (drop 1 vs1) ++ map3 (,,) vs2 (drop 1 vs1) (drop 1 vs2)

matRow : Float -> Float -> Float -> Float -> Float -> List Float -> List Vertex
matRow x pos_dx coord_dx ymul z =
  let m posOffset coordOffset ys0 = case ys0 of
          (y::ys) -> { pos = vec3 posOffset (y*ymul) z, coord = vec3 coordOffset z 0 } :: (m (posOffset + pos_dx) (coordOffset + coord_dx) ys)
          _       -> []
  in
      m 0.0 0.0

-- Todo: turn this into something that takes an Array2D of (Float (elevation), color) and
-- produces a Drawable ColorVertex
-- ... then, color the input according to elevation (eg. water, grass, snow etc.)
--  and gradient (rocks, cliffs), and both (beaches)

surfaceMeshArr : Float -> Float -> Float -> Float -> Float -> Float -> Array2D Float -> Drawable Vertex
surfaceMeshArr x dx_pos dx_coord ymul z dz arr0 =
    surfaceMesh x dx_pos dx_coord ymul z dz (Array2D.toLists arr0)

surfaceMesh : Float -> Float -> Float -> Float -> Float -> Float -> List (List Float) -> Drawable Vertex
surfaceMesh x dx_pos dx_coord ymul z dz m =
    let
        zs = indexedMap (\ix _ -> z + dz * toFloat ix) m
        rows = List.map2 (matRow x dx_pos dx_coord ymul) zs m
    in
        Triangle <| List.concat <| List.map2 mkStrip rows (drop 1 rows)

matList : Float -> List Float -> List Vertex
matList z =
  let m posOffset coordOffset xs0 = case xs0 of
          (x::xs) -> { pos = vec3 posOffset (x*10) z, coord = vec3 coordOffset z 0 } :: (m (posOffset + 0.4) (coordOffset + 0.1) xs)
          _       -> []
  in
      m 0.0 0.0
