module Things.Terrain where

import List.Extra exposing (splitAt)

import Array
import Array2D exposing (Array2D)
import Math.Vector3 exposing (..)
import Util exposing (..)
import Zipper2D exposing (Zipper2D, map)

import Things.Surface2D exposing (..)

import Engine exposing (..)

mountains : Array2D Float -> Array2D (Float, Vec3)
mountains arr0 =
  let green h = hslToVec3
          (degrees (70 + toFloat (round ((h+0.34)*500) % 70)))
          (0.3 + h/4)
          (0.2 + (1-h)/3)
      blue h = hslToVec3 (degrees 196) 0.8 ((h+0.1)*4)
      sea h = hslToVec3 (degrees 190) 0.8 ((abs (h/10) + 0.1)*3)
      snow h = hslToVec3 (degrees 178) 0.8 h
      paint h =
          if h > 0.8 then (h, snow h)
          else if h < 0.0 then (0.1, sea h)
          else if h < 0.1 then (0.1, blue h)
          else (h, green h)
  in Array2D.map paint arr0

tileTerrain : Int -> Array2D (Float,Vec3) -> List (List ((List (List (Float, Vec3)), (Int, Int))))
tileTerrain smallSide arr0 = case arr0 of
  Array2D.Array2D bigSide _ ->
    let coords = subSquares smallSide bigSide
    in List.map (List.map (mkTile smallSide arr0)) coords

mkTile : Int -> Array2D (Float,Vec3) -> (Int, Int) -> (List (List (Float, Vec3)), (Int, Int))
mkTile smallSide arr0 (x0, y0) = case arr0 of
  Array2D.Array2D bigSide arr ->
    let extent x = min (x+smallSide+1) (bigSide - 1)
        slice x y = Array.toList <| Array.slice (x + y*bigSide) (extent x + y*bigSide) arr
        rows = List.map (slice x0) [y0 .. extent y0]
        out = List.reverse <| List.foldl (::) [] rows
    in (out, (x0, y0))

-- placeTerrain : List (List (Array2D (Float, Vec3), (Int, Int))) -> Array2D Thing
placeTerrain terrainsCoords =
    let
        terrainSurfacesCoords = List.map (List.map (\(t,xy) -> (surface2D t, xy))) terrainsCoords
        terrainz = Array2D.fromLists terrainSurfacesCoords
    in
        Array2D.map (\(s,(x,z)) -> { s | pos = vec3 (toFloat x*2) 0 (toFloat z*2)}) terrainz
