module Math.Procedural where

import Array
import Array2D exposing (..)
import Random
import Random.Array
import Util exposing (subSquares, unfoldWhile)

----------------------------------------------------------------------

-- Generate random terrain of side length side
randTerrain2D : Int -> Random.Generator (Array2D Float)
randTerrain2D side =
    let s2 = side - 1 - (side%8) in
      Random.map (
          terrain2D side <<
          setXY 0   0 1.0 <<
          setXY 0  s2 0.8 <<
          setXY s2 0 1.0 <<
          setXY s2 s2 0.5
      ) <|
      Random.map Array2D.fromArray <|
      Random.Array.array (side*side) (Random.float (-1.0) 1.0)

----------------------------------------------------------------------

simpleTerrain2D : Int -> Array2D Float
simpleTerrain2D side = terrain2D side (fromArray (Array.repeat (side*side) 1.0))

----------------------------------------------------------------------

-- TODO: vary the fractal nature (h -> 0, range -> 0), so it slopes more smoothly to the sea

-- Given an input 2D array of floats in the range [-1.0 .. 1.0],
-- generate terrain
terrain2D : Int -> Array2D Float -> Array2D Float
terrain2D side arr0 =
    let h = 0.3
        range = 0.5
        sizesRanges = unfoldWhile (\(x,r) -> (x//2,r*h)) (\(x,r) -> x > 1) (side, range)
    in  List.foldl (allQuads side) arr0 sizesRanges

-- Generate terrain for all quads of given size
allQuads : Int -> (Int, Float) -> Array2D Float -> Array2D Float
allQuads side (size, range) arr0 =
    let coords = List.concat <| subSquares size side
    in List.foldl (quad side size range) arr0 coords

    -- For each value: side, side/2, side/4 ... 3,
    -- call quad on each quadrant of that side length

quad : Int -> Int -> Float -> (Int, Int) -> Array2D Float -> Array2D Float
quad side len range (x,y) arr = 
    let
        tL = getXY x y 0 arr
        tR = getXY (x+len) y 0 arr
        bL = getXY x (y+len) 0 arr
        bR = getXY (x+len) (y+len) 0 arr

        l2 = len // 2

        cR = range * getXY (x+l2) (y+l2) 0 arr
        nR = range * getXY (x+l2) y 0 arr
        eR = range * getXY x (y+l2) 0 arr
        wR = range * getXY (x+len) (y+l2) 0 arr
        sR = range * getXY (x+l2) (y+len) 0 arr

    in
        setXY (x+l2) (y+l2) (((tL + tR + bL + bR) / 4) + cR) <|
        setXY (x+l2) y (((tL + tR) / 2) + nR) <|
        setXY x (y+l2) (((tR + bR) / 2) + eR) <|
        setXY (x+len) (y+l2) (((tL + bL) / 2) + wR) <|
        setXY (x+l2) (y+len) (((bL + bR) / 2) + sR) <|
        arr

markQuad : Int -> Float -> (Int, Int) -> Array2D Float -> Array2D Float
markQuad len range (x,y) arr = 
    let
        l2 = len // 2
    in
        setXY (x+l2) (y+l2) range <|
        setXY (x+l2) y range <|
        setXY x (y+l2) range <|
        setXY (x+len) (y+l2) range <|
        setXY (x+l2) (y+len) range <|
        arr

{-

0 . 2 . 1 . 2 . 0
. . . . . . . . .
2 . 2 . 2 . 2 . 2
. . . . . . . . .
1 . 2 . 1 . 2 . 1
. . . . . . . . .
2 . 2 . 2 . 2 . 2
. . . . . . . . .
0 . 2 . 1 . 2 . 0

-}
