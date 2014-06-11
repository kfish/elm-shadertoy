module Display.VolSurface (cloudsVolSurface, fogMountainsVolSurface, voronoiDistancesVolSurface, volSurface) where

import Math.Vector2 (Vec2)
import Math.Vector3 (..)
import Math.Matrix4 (..)
import Graphics.WebGL (..)

import Shaders.Clouds (clouds)
import Shaders.Fire (fire)
import Shaders.FogMountains (fogMountains)
--import Shaders.SimplePlasma (simplePlasma)
import Shaders.VoronoiDistances (voronoiDistances)
import Shaders.WorldVertex (Vertex, worldVertex)

import Model

cloudsVolSurface : (Int,Int) -> Time -> Mat4 -> Entity
cloudsVolSurface = volSurface worldVertex clouds

fogMountainsVolSurface : (Int,Int) -> Time -> Mat4 -> Entity
fogMountainsVolSurface = volSurface worldVertex fogMountains

voronoiDistancesVolSurface : (Int,Int) -> Time -> Mat4 -> Entity
voronoiDistancesVolSurface = volSurface worldVertex voronoiDistances

-- volSurface : Shader attributes uniforms varying -> Shader {} uniforms varyings
--    -> (Int,Int) -> Time -> Mat4 -> Entity
volSurface vertexShader fragmentShader (w,h) t view =
    let resolution = vec3 (toFloat w) (toFloat h) 0
        s = inSeconds t
    in
        entity vertexShader fragmentShader volSurfaceMesh
            { iResolution=resolution, iGlobalTime=s, view=view }

unfold : Int -> (a -> a) -> a -> [a]
unfold n f x = if n==0 then [] else
  let res=f x in (res :: unfold (n-1) f res)

zip3 : [a] -> [b] -> [c] -> [(a,b,c)]
zip3 xs ys zs =
  case (xs, ys, zs) of
    (x::xs', y::ys', z::zs') -> (x,y,z) :: zip3 xs' ys' zs'
    _ -> []

rotY n = makeRotate (2*pi/n) (vec3 0 1 0)
rotZ n = makeRotate (-2*pi/n) (vec3 0 0 1)

rotBoth : Float -> Vertex -> Vertex
rotBoth n x = { position = transform (rotY n) x.position, coord = transform (rotZ n) x.coord }

seven : Vertex -> [Vertex]
seven = unfold 7 (rotBoth 8)

eights x = let x7 = seven x in (x::x7, x7++[x])

matList : Float -> [Float] -> [Vertex]
matList z =
  let m posOffset coordOffset xs0 = case xs0 of
          (x::xs) -> { position = vec3 posOffset (x*10) z, coord = vec3 coordOffset z 0 } :: (m (posOffset + 0.4) (coordOffset + 0.1) xs)
          _       -> []
  in
      m 0.0 0.0

volSurfaceMesh : [Triangle Vertex]
volSurfaceMesh =
  let
      yOffset = 1.21
      yMul = -4.2
      -- Vertices
      table0 = { position = vec3 0 0 0, coord = vec3 0 (yMul*(0.0-yOffset)) 0 }
      tableV = { position = vec3 0.57 0 0, coord = vec3 0 (yMul*(0.57-yOffset)) 0 }
      (tableVS0, tableVS1) = eights tableV

      facetY = -0.2
      facet0 = rotBoth -16 { position = vec3 0.8 facetY 0, coord = vec3 0.2 (yMul*(0.8-yOffset)) 0 }
      (facetVS0, facetVS1) = eights facet0

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

      mkStrip : [Vertex] -> [Vertex] -> [Triangle Vertex]
      mkStrip vs1 vs2 = zip3 vs1 vs2 (drop 1 vs1) ++ zip3 vs2 (drop 1 vs1) (drop 1 vs2)

      -- Triangles

  in
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

