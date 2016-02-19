module Things.Terrain where

import List.Extra exposing (splitAt)

import Array2D exposing (Array2D)
import Math.Vector3 exposing (..)
import Util exposing (repeatedly, splitEvery)
import Zipper2D exposing (Zipper2D, map)

import Display.VolSurface exposing (..)

import Engine exposing (..)

-- placeTerrains : List (Array2D Float) -> Zipper2D Thing
placeTerrain terrains =
    let
        terrainSurfacesCoords = splitEveryWithCoords 8 <| List.map testSurfaceArr terrains
        terrainz = Zipper2D.fromLists terrainSurfacesCoords
    in
        Zipper2D.map (\(s,(x,z)) -> { s | pos = vec3 (toFloat x*10) (-3) (toFloat z*10)}) terrainz

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
