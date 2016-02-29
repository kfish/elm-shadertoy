module Things.Terrain where

import List.Extra exposing (splitAt)
import Math.Matrix4 as M4
import WebGL exposing (..)

import Array
import Array2D exposing (Array2D)
import Math.Vector3 exposing (..)
import Util exposing (..)
import Zipper2D exposing (Zipper2D, map)

import Things.Surface2D exposing (..)

import Engine exposing (..)

visibleTerrain : Array2D Thing -> List Thing
visibleTerrain arr =
    let
        sees = Array2D.map (\(Thing pos _ see) -> (tview (M4.translate pos) see)) arr
    in
        List.map extractThing
            [{ pos = vec3 0 0 0, orientation = vec3 1 0 1, see = seeTerrain sees }]

-- CONGRATS! You've successfully plumbed a Perception into terrain, making the
-- terrain itself into a Thing. Now, all you need to do is add the camera pos
-- to Perception, and you'll be able to do "radius 8" on the terrain.
-- ... THEN ... go back and plumb dt into demoThings, so you can get the boids
-- and balls working again.
-- ... THEN, trivially add back the static objects like cubes and diamonds
-- that don't have a signal input.
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

mountains : Array2D Float -> Array2D NoiseSurfaceVertex
mountains arr0 =
  let green h = hslToVec3
          (degrees (70 + toFloat (round ((h+0.34)*500) % 70)))
          (0.3 + h/4)
          (0.2 + (1-h)/3)
      blue h = hslToVec3 (degrees 196) 0.8 ((h+0.1)*4)
      sand h = hslToVec3 (degrees 50) 0.8 ((h+0.1)*4)
      sea h = hslToVec3 (degrees 190) 0.8 ((abs (h/10) + 0.1)*3)
      snow h = hslToVec3 (degrees 178) 0.8 h
      paint h =
          if h > 0.8 then (h, snow h, 0.8, 0.0, 0.3)
          else if h < 0.0 then (0.1, sea h, 1.0, 0.7, 0.5)
          else if h < 0.1 then (0.1, blue h, 1.0, 0.7, 0.5)
          else if h < 0.15 then (h, sand h, 80.0, 0.0, 0.7)
          else (h, green h, 0.8, 0.001, 0.3)
  in Array2D.map paint arr0

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
