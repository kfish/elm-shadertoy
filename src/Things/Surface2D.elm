module Things.Surface2D
    ( SurfaceVertex, surface2D
    , NoiseSurfaceVertex, noiseSurface2D, rippleNoiseSurface2D
    , Placement, defaultPlacement
    ) where

import Array
import Array2D exposing (Array2D)
import List exposing (..)
import List.Extra exposing (splitAt)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Vector4 exposing (Vec4)
import Math.Matrix4 exposing (..)
import Maybe.Extra exposing (isJust)
import Time exposing (Time)
import WebGL exposing (..)

import Shaders.ColorFragment exposing (..)
import Shaders.NoiseVertex exposing (..)

import Model

type alias Placement =
    { xOffset : Float
    , xDelta  : Float
    , yOffset : Float
    , yMult   : Float
    , zOffset : Float
    , zDelta  : Float
    , tileSize : Int -- Length of side of a square tile
    , bigSide : Int
    }

type alias SurfaceVertex = (Float, Vec3)

-- (height, color, textureScale, timeScale, smoothing)
type alias NoiseSurfaceVertex = (Float, Vec4, Float, Float, Float)

defaultPlacement : Placement
defaultPlacement =
    { xOffset = -256
    , xDelta  = 2
    , yOffset = 0
    , yMult   = 80
    , zOffset = -256
    , zDelta  = 2
    , tileSize = 8
    , bigSide = 1024
    }

toNSV (y,rgb) = (y, rgb, 0.0, 0.0, 0.0)

surface2D placement xz = surface noiseVertex noiseColorFragment
    << surfaceMesh xz placement
    << List.map (List.map toNSV)

noiseSurface2D placement xz = surface noiseVertex noiseColorFragment
    << surfaceMesh xz placement

surface vertexShader fragmentShader mesh =
    let see = seeSurface vertexShader fragmentShader mesh
    in { pos = vec3 0 0 0, orientation = vec3 1 0 1, see = see }

seeSurface vertexShader fragmentShader mesh p =
    let (w,h) = p.resolution
        resolution = vec3 (toFloat w) (toFloat h) 0
        s = p.globalTime
        detail = p.measuredFPS / 3.0
    in
        [render vertexShader fragmentShader mesh
            { iResolution=resolution, iDetail=detail, iGlobalTime=s, iGlobalTimeV=s, view=p.viewMatrix }]


rippleNoiseSurface2D ripple placement xz = rippleSurface noiseVertex noiseColorFragment ripple
    << surfaceMeshMaybe xz placement

rippleSurface vertexShader fragmentShader ripple mesh =
    let see = rippleSeeSurface vertexShader fragmentShader ripple mesh
    in { pos = vec3 0 0 0, orientation = vec3 1 0 1, see = see }

rippleSeeSurface vertexShader fragmentShader ripple mesh p =
    let (w,h) = p.resolution
        resolution = vec3 (toFloat w) (toFloat h) 0
        s = p.globalTime
        detail = p.measuredFPS / 3.0
    in
        [render vertexShader fragmentShader mesh
            { iResolution=resolution, iDetail=detail, iGlobalTime=s, iGlobalTimeV=s, view=p.viewMatrix, iRipple=ripple }]

----------------------------------------------------------------------

mkStrip : List v -> List v -> List (v, v, v)
mkStrip vs1 vs2 = map3 (,,) vs1 vs2 (drop 1 vs1) ++ map3 (,,) vs2 (drop 1 vs1) (drop 1 vs2)

matRow : (Float,Float) -> Placement -> Float -> List NoiseSurfaceVertex -> List NoiseVertex
matRow (rx,rz) placement z =
  let m posOffset coordOffset ys0 = case ys0 of
          ((y,rgb,tex,tim,smoo)::ys) -> ({ pos = vec3 (placement.xOffset+posOffset) (placement.yOffset+y*placement.yMult) z, color = rgb, coord = vec3 coordOffset (rz+z) 0, textureScale = tex, timeScale = tim, smoothing = smoo }) :: (m (posOffset + placement.xDelta) (coordOffset + placement.xDelta) ys)
          _       -> []
  in
      m 0.0 rx

surfaceMesh : (Float,Float) -> Placement -> List (List NoiseSurfaceVertex)
    -> Drawable NoiseVertex
surfaceMesh (rx,rz) placement m =
    let
        zs = indexedMap (\ix _ -> placement.zOffset + placement.zDelta * toFloat ix) m
        rows = List.map2 (matRow (rx,rz) placement) zs m
    in
        Triangle <| List.concat <| List.map2 mkStrip rows (drop 1 rows)

----------------------------------------------------------------------

mkStripMaybe : List (Maybe v) -> List (Maybe v) -> List (v, v, v)
mkStripMaybe vs1 vs2 =
    let
        mkMaybe triangle = case triangle of
            (Just v1, Just v2, Just v3) -> Just (v1, v2, v3)
            _                           -> Nothing
        strip = map3 (,,) vs1 vs2 (drop 1 vs1) ++ map3 (,,) vs2 (drop 1 vs1) (drop 1 vs2)
    in
        List.filterMap mkMaybe strip

matRowMaybe : (Float,Float) -> Placement -> Float -> List (Maybe NoiseSurfaceVertex) -> List (Maybe NoiseVertex)
matRowMaybe (rx,rz) placement z =
  let m posOffset coordOffset ys0 = case ys0 of
          ((Just (y,rgb,tex,tim,smoo))::ys) -> (Just { pos = vec3 (placement.xOffset+posOffset) (placement.yOffset+y*placement.yMult) z, color = rgb, coord = vec3 coordOffset (rz+z) 0, textureScale = tex, timeScale = tim, smoothing = smoo }) :: (m (posOffset + placement.xDelta) (coordOffset + placement.xDelta) ys)
          (Nothing::ys) -> Nothing :: (m (posOffset + placement.xDelta) (coordOffset + placement.xDelta) ys)
          _       -> []
  in
      m 0.0 rx

surfaceMeshMaybe : (Float,Float) -> Placement -> List (List (Maybe NoiseSurfaceVertex))
    -> Drawable NoiseVertex
surfaceMeshMaybe (rx,rz) placement m =
    let
        zs = indexedMap (\ix _ -> placement.zOffset + placement.zDelta * toFloat ix) m
        rows = List.map2 (matRowMaybe (rx,rz) placement) zs m
    in
        Triangle <| List.concat <| List.map2 mkStripMaybe rows (drop 1 rows)
