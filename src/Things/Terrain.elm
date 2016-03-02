module Things.Terrain (elevation, bounds, paint, mountains) where

import List.Extra exposing (splitAt)
import Math.Matrix4 as M4
import Math.Vector3 as V3
import WebGL exposing (..)

import Array
import Array2D exposing (Array2D)
import Math.Vector3 exposing (..)
import Util exposing (..)
import Zipper2D exposing (Zipper2D, map)

import Things.Surface2D exposing (..)

import Engine exposing (..)

----------------------------------------------------------------------

mountains : Float -> NoiseSurfaceVertex
mountains h =
  let green = hslToVec3
          (degrees (70 + toFloat (round ((h+0.34)*500) % 70)))
          (0.3 + h/4)
          (0.2 + (1-h)/3)
      blue = hslToVec3 (degrees 196) 0.8 ((h+0.1)*4)
      sand = hslToVec3 (degrees 50) 0.8 ((h+0.1)*4)
      sea = hslToVec3 (degrees 190) 0.8 ((abs (h/10) + 0.1)*3)
      snow = hslToVec3 (degrees 178) 0.8 h
  in
      if h > 0.8 then (h, snow, 0.8, 0.0, 0.3)
      else if h < 0.0 then (0.1, sea, 1.0, 0.7, 0.5)
      else if h < 0.1 then (0.1, blue, 1.0, 0.7, 0.5)
      else if h < 0.15 then (h, sand, 80.0, 0.0, 0.7)
      else (h, green, 0.8, 0.001, 0.3)

----------------------------------------------------------------------

{-
elevation : Array2D Float -> Vec3 -> Float
elevation terrain pos =
    let
        ix0 = floor <| (getX pos + 256) / 2
        iz0 = floor <| (getZ pos + 256) / 2
        getXZ x z = (Array2D.getXY x z 0 terrain) * 80
    in
        getXZ ix0 iz0
-}

-- Elevation of terrain at a given coordinate
-- Linearly interpolated on the mesh triangle
-- TODO: move this to the Terrain module
elevation : Array2D Float -> Vec3 -> Float
elevation terrain pos =
    let
        ix0 = (getX pos + 256) / 2
        ix  = floor ix0
        ixf = ix0 - toFloat ix

        iz0 = (getZ pos + 256) / 2
        iz  = floor iz0
        izf = iz0 - toFloat iz

        getXZ x z = (Array2D.getXY x z 0 terrain) * 80

        i00 = getXZ ix     iz      --     00 ... 10  -> x
        i10 = getXZ (ix+1) iz      --  |  .    /  .
                                   --  v  .   /   .
        i01 = getXZ ix     (iz+1)  --     .  /    .
        i11 = getXZ (ix+1) (iz+1)  --  z  01 ... 11

        mix a b f = (a * (1-f) + b * f) / 2 -- f describes how close to a

    in
        if ixf + izf < 1.0 then
            mix i00 i10 ixf + mix i00 i01 izf
        else
            mix i01 i11 ixf + mix i10 i11 izf

bounds : Vec3 -> Vec3
bounds pos =
    let bound x low high = if (x < low) then low else (if x > high then high else x)
        (x,y,z) = V3.toTuple pos
    in vec3 (bound x -246 1782) (bound y 0 1000) (bound z -246 1782)

            
----------------------------------------------------------------------

paint : (Float -> NoiseSurfaceVertex) -> Array2D Float -> List Thing
paint how terrain =
       visibleTerrain
    <| terrainGrid
    <| Array2D.map how terrain

visibleTerrain : Array2D Thing -> List Thing
visibleTerrain arr =
    let
        sees = Array2D.map (\(Thing pos _ see) -> (tview (M4.translate pos) see)) arr
    in
        List.map extractThing
            [{ pos = vec3 0 0 0, orientation = vec3 1 0 1, see = seeTerrain sees }]

seeTerrain : Array2D See -> See
seeTerrain sees p =
       List.concat
    <| List.map (\see -> see p)
    <| nearby p.cameraPos sees

nearby : Vec3 -> Array2D See -> List See
nearby pos sees =
    let
        ix0 = floor ((getX pos + 256) / (2*8))
        iz0 = floor ((getZ pos + 256) / (2*8))
        getXZ x z = Array2D.getXY z x (\_ -> []) sees

        -- The visible radius of tiles depends on the height of the camera
        r = max 8 (floor ((getY pos) / 10))
        ir = iradius r
    in
        List.map (\(x,y) -> getXZ (ix0+x) (iz0+y)) ir

terrainGrid = placeTerrain << tileTerrain 8

-- TODO: break out a (Float -> NoiseSurfaceVertex) paint function, and
-- pass this to a function that makes a [Thing] out of a terrain : Array2D Float
-- ... then, that function can make use of the passed-in terrain to take elevation
-- into account for nearby

tileTerrain : Int -> Array2D NoiseSurfaceVertex
    -> List (List ((List (List NoiseSurfaceVertex), (Int, Int))))
tileTerrain smallSide arr0 = case arr0 of
  Array2D.Array2D bigSide _ ->
    let coords = subSquares smallSide bigSide
    in List.map (List.map (mkTile smallSide arr0)) coords

mkTile : Int -> Array2D NoiseSurfaceVertex -> (Int, Int) -> (List (List NoiseSurfaceVertex), (Int, Int))
mkTile smallSide arr0 (x0, y0) = case arr0 of
  Array2D.Array2D bigSide arr ->
    let extent x = min (x+smallSide+1) (bigSide - 1)
        slice x y = Array.toList <| Array.slice (x + y*bigSide) (extent x + y*bigSide) arr
        rows = List.map (slice x0) [y0 .. extent (y0-1)]
        out = List.reverse <| List.foldl (::) [] rows
    in (out, (x0, y0))

-- placeTerrain : List (List ((List (List NoiseSurfaceVertex)), (Int, Int))) -> Array2D Thing
placeTerrain terrainsCoords =
    let
        terrainSurfacesCoords = List.map (List.map (\(t,xy) -> (noiseSurface2D t, xy))) terrainsCoords
        terrainz = Array2D.fromLists terrainSurfacesCoords
    in
        Array2D.map (\(s,(x,z)) -> extractThing { s | pos = vec3 (toFloat x*2) 0 (toFloat z*2)}) terrainz

-- The x*2, z*2 numbers above must match the arguments to surfaceMesh. Put these
-- into a record and pass it to both functions, and also to visibleTerrain
