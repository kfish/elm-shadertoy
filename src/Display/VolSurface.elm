module Display.VolSurface (testSurface, testSurfaceArr, cloudsVolSurface, fogMountainsVolSurface, voronoiDistancesVolSurface, volSurface) where

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

-- cloudsVolSurface : Signal Thing
cloudsVolSurface = Signal.constant <| volSurface worldVertex clouds

-- fogMountainsVolSurface : Signal Thing
fogMountainsVolSurface = Signal.constant <| volSurface worldVertex fogMountains

-- voronoiDistancesVolSurface : (Int,Int) -> Time -> Mat4 -> Renderable
-- voronoiDistancesVolSurface : Signal Thing
voronoiDistancesVolSurface = Signal.constant <| volSurface worldVertex voronoiDistances

-- volSurface : Shader attributes uniforms varying -> Shader {} uniforms varyings
--    -> (Int,Int) -> Time -> Mat4 -> Renderable 
volSurface vertexShader fragmentShader =
    let see = seeVolSurface vertexShader fragmentShader
    in { pos = vec3 0 0 0, orientation = vec3 1 0 1, see = see }

seeVolSurface vertexShader fragmentShader p =
    let (w,h) = p.resolution
        resolution = vec3 (toFloat w) (toFloat h) 0
        s = inSeconds p.globalTime
    in
        [render vertexShader fragmentShader volSurfaceMesh
            { iResolution=resolution, iGlobalTime=s, view=p.viewMatrix }]

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

testSurfaceMeshArr = surfaceMeshArr -10 1.4 1.1 10 0 1

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

volSurfaceMesh : Drawable Vertex
volSurfaceMesh =
  let

      mat0 = matList 0 [1.0, 0.8, 0.7, 0.4, 0.3, 0.1, 0.2, 0.5, 0.6, 1.0]
      mat1 = matList 1 [0.9, 0.75, 0.63, 0.3, 0.21, 0.2, 0.23, 0.4, 0.56, 0.8]
      mat2 = matList 2 [0.8, 0.65, 0.53, 0.2, 0.11, 0.23, 0.18, 0.3, 0.46, 0.6]

      -- 30
      mat30 = matList 0 [0.47963, 0.47963, 0.47963, 0.47963, 0.47963, 0.45415, 0.40017, 0.38271,
                         0.33768, 0.27504, 0.21805, 0.16494, 0.11874, 0.11559, 0.09467, 0.10513,
                         0.09494, 0.09494, 0.09494, 0.09494, 0.09494, 0.09494, 0.09494, 0.09494,
                         0.09494, 0.09494]

      -- 60
      mat60 = matList 1 [0.39204, 0.39204, 0.39204, 0.39204, 0.39204, 0.38438, 0.3582, 0.32115,
                         0.27873, 0.23803, 0.19761, 0.16026, 0.12376, 0.12451, 0.09887, 0.10203,
                         0.10182, 0.10182, 0.10182, 0.10182, 0.10182, 0.10182, 0.10182, 0.10182,
                         0.10182, 0.10182]

      -- 90
      mat90 = matList 2 [0.35587, 0.35587, 0.35587, 0.35587, 0.35587, 0.35587, 0.32689, 0.29207,
                         0.2563, 0.22268, 0.19054, 0.15972, 0.12949, 0.12946, 0.10583, 0.10178,
                         0.10178, 0.10178, 0.10178, 0.10178, 0.10178, 0.10178, 0.10178, 0.10178,
                         0.10178, 0.10178]

      -- 120
      mat120 = matList 3 [0.32882, 0.32882, 0.32882, 0.32689, 0.32246, 0.31887, 0.30407, 0.27367,
                          0.24272, 0.21382, 0.18646, 0.15964, 0.13292, 0.13395, 0.11151, 0.10308,
                          0.1022, 0.1022, 0.1022, 0.1022, 0.1022, 0.1022, 0.1022, 0.1022,
                          0.1022, 0.1022]

      -- 150
      mat150 = matList 4 [0.34674, 0.34674, 0.34674, 0.33973, 0.32327, 0.30944, 0.28951, 0.2624,
                          0.23536, 0.20993, 0.1853, 0.16104, 0.13711, 0.13801, 0.11719, 0.10579,
                          0.10329, 0.10329, 0.10329, 0.10329, 0.10329, 0.10329, 0.10329, 0.10329,
                          0.10329, 0.10329]

      -- 180
      mat180 = matList 5 [0.35819, 0.35819, 0.35819, 0.34803, 0.32381, 0.30299, 0.27939, 0.2546,
                          0.23032, 0.2073, 0.18452, 0.16197, 0.13983, 0.14065, 0.12084, 0.10756,
                          0.10401, 0.10401, 0.10401, 0.10401, 0.10401, 0.10401, 0.10401, 0.10401,
                          0.10401, 0.10401]

      -- 360
      mat360 = matList 6 [0.3386, 0.33268, 0.32502, 0.30708, 0.28734, 0.26873, 0.2508, 0.23376,
                          0.21721, 0.20086, 0.18483, 0.1689, 0.15357, 0.15133, 0.13696, 0.12449,
                          0.11532, 0.11117, 0.11044, 0.11044, 0.11044, 0.11044, 0.11044, 0.11044,
                          0.11044, 0.11044]

      -- 720
      mat720 = matList 7 [0.32204, 0.30433, 0.28791, 0.27287, 0.2589, 0.24581, 0.23323, 0.22123,
                          0.20944, 0.19775, 0.18637, 0.17542, 0.16483, 0.16844, 0.15857, 0.14942,
                          0.1413, 0.13443, 0.12956, 0.12553, 0.12379, 0.12538, 0.12697, 0.12857,
                          0.13018, 0.13168]

      -- 1080
      mat1080 = matList 8 [0.3051, 0.28581, 0.27137, 0.25878, 0.24694, 0.23559, 0.22481, 0.2147,
                           0.20408, 0.19383, 0.18426, 0.17483, 0.16549, 0.17929, 0.17074, 0.16278,
                           0.15549, 0.14913, 0.14374, 0.13932, 0.13708, 0.13708, 0.13708, 0.13708,
                           0.13708, 0.13708]

  in

    Triangle <|
      --mkStrip mat0 mat1 ++
      --mkStrip mat1 mat2 ++

      mkStrip mat30 mat60 ++
      mkStrip mat60 mat90 ++
      mkStrip mat90 mat120 ++
      mkStrip mat120 mat150 ++
      mkStrip mat150 mat180 ++
      mkStrip mat180 mat360 ++
      mkStrip mat360 mat720 ++
      mkStrip mat720 mat1080 ++
      []

