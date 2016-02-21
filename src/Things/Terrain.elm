module Things.Terrain where

import List.Extra exposing (splitAt)

import Array
import Array2D exposing (Array2D)
import Math.Vector3 exposing (..)
import Util exposing (..)
import Zipper2D exposing (Zipper2D, map)

import Things.Surface2D exposing (..)

import Engine exposing (..)

tileTerrain : Int -> Array2D Float -> List (List ((Array2D Float, (Int, Int))))
tileTerrain smallSide arr0 = case arr0 of
  Array2D.Array2D bigSide _ ->
    let coords = subSquares smallSide bigSide
    in List.map (List.map (mkTile smallSide arr0)) coords

mkTile : Int -> Array2D Float -> (Int, Int) -> (Array2D Float, (Int, Int))
mkTile smallSide arr0 (x0, y0) = case arr0 of
  Array2D.Array2D bigSide arr ->
    let extent x = if x+smallSide == bigSide then x+smallSide else x+smallSide+1
        slice x y = Array.slice (x + y*bigSide) (extent x + y*bigSide) arr
        rows = List.map (slice x0) [y0 .. extent y0-1]
        out = Array2D.Array2D (smallSide+1) <| List.foldl (flip Array.append) Array.empty rows
    in (out, (x0, y0))

-- placeTerrain : List (Array2D Float) -> Zipper2D Thing
placeTerrain terrainsCoords =
    let
        terrainSurfacesCoords = List.map (List.map (\(t,xy) -> (testSurfaceArr t, xy))) terrainsCoords
        terrainz = Zipper2D.fromLists terrainSurfacesCoords
    in
        Zipper2D.map (\(s,(x,z)) -> { s | pos = vec3 (toFloat x*1.6) (-70) (toFloat z*1.6)}) terrainz

splitEveryWithCoords : Int -> List a -> List (List (a, (Int,Int)))
splitEveryWithCoords size xs0 =
    let
        izs = [0..size]
        go ix xs = case xs of
            [] -> []
            _  -> let (p,q) = splitAt size xs in
                      List.map2 (\val iz -> (val, (ix,iz))) p izs :: go (ix+1) q
    in
        go 0 xs0
