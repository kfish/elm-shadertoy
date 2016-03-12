module Things.Surface2D (SurfaceVertex, NoiseSurfaceVertex, surface2D, noiseSurface2D) where

import Array
import Array2D exposing (Array2D)
import List exposing (..)
import List.Extra exposing (splitAt)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Vector4 exposing (Vec4)
import Math.Matrix4 exposing (..)
import Maybe.Extra exposing (isJust)
import Time exposing (Time, inSeconds)
import WebGL exposing (..)

import Shaders.ColorFragment exposing (..)
import Shaders.NoiseVertex exposing (..)

import Model

type alias SurfaceVertex = (Float, Vec3)

-- (height, color, textureScale, timeScale, smoothing)
type alias NoiseSurfaceVertex = (Float, Vec4, Float, Float, Float)

toNSV (y,rgb) = (y, rgb, 0.0, 0.0, 0.0)

surface2D (x,z) = surface noiseVertex noiseColorFragment 0.0
    << fromListsDefaults (x,z)
    << List.map (List.map (Maybe.map toNSV))

noiseSurface2D ripple (x,z) = surface noiseVertex noiseColorFragment ripple << fromListsDefaults (x,z)

surface vertexShader fragmentShader ripple mesh =
    let see = seeSurface vertexShader fragmentShader ripple mesh
    in { pos = vec3 0 0 0, orientation = vec3 1 0 1, see = see }

seeSurface vertexShader fragmentShader ripple mesh p =
    let (w,h) = p.resolution
        resolution = vec3 (toFloat w) (toFloat h) 0
        s = inSeconds p.globalTime
    in
        [render vertexShader fragmentShader mesh
            { iResolution=resolution, iGlobalTime=s, iGlobalTimeV=s, view=p.viewMatrix, iRipple=ripple }]

fromListsDefaults xz = surfaceMesh xz -256 2 2 0 80 -256 2

----------------------------------------------------------------------

mkStrip : List (Maybe v) -> List (Maybe v) -> List (v, v, v)
mkStrip vs1 vs2 =
    let
        mkMaybe triangle = case triangle of
            (Just v1, Just v2, Just v3) -> Just (v1, v2, v3)
            _                           -> Nothing
        strip = map3 (,,) vs1 vs2 (drop 1 vs1) ++ map3 (,,) vs2 (drop 1 vs1) (drop 1 vs2)
    in
        List.filterMap mkMaybe strip

matRow : (Float,Float) -> Float -> Float -> Float -> Float -> Float -> Float -> List (Maybe NoiseSurfaceVertex) -> List (Maybe NoiseVertex)
matRow (rx,rz) x pos_dx coord_dx y0 ymul z =
  let m posOffset coordOffset ys0 = case ys0 of
          ((Just (y,rgb,tex,tim,smoo))::ys) -> (Just { pos = vec3 (x+posOffset) (y0+y*ymul) z, color = rgb, coord = vec3 coordOffset (rz+z) 0, textureScale = tex, timeScale = tim, smoothing = smoo }) :: (m (posOffset + pos_dx) (coordOffset + coord_dx) ys)
          (Nothing::ys) -> Nothing :: (m (posOffset + pos_dx) (coordOffset + coord_dx) ys)
          _       -> []
  in
      m 0.0 rx

surfaceMesh : (Float,Float) -> Float -> Float -> Float -> Float -> Float -> Float -> Float
    -> List (List (Maybe NoiseSurfaceVertex))
    -> Drawable NoiseVertex
surfaceMesh (rx,rz) x dx_pos dx_coord y0 ymul z dz m =
    let
        zs = indexedMap (\ix _ -> z + dz * toFloat ix) m
        rows = List.map2 (matRow (rx,rz) x dx_pos dx_coord y0 ymul) zs m
    in
        Triangle <| List.concat <| List.map2 mkStrip rows (drop 1 rows)
