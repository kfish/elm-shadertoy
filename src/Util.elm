module Util where

import List.Extra exposing (splitAt)

import Color exposing (hsl, toRgb)
import Math.Vector3 exposing (..)

---------------------------------------------------------------------

splitEvery : Int -> List a -> List (List a)
splitEvery size xs = case xs of
    [] -> []
    _  -> let (p,q) = splitAt size xs in p :: splitEvery size q

unfoldWhile : (a -> a) -> (a -> Bool) -> a -> List a
unfoldWhile f p x = if p x then x :: unfoldWhile f p (f x) else []

repeatedly : Int -> (a -> a) -> a -> a
repeatedly n f x = if n <= 0 then x else repeatedly (n-1) f (f x)

-- Generate top-left coords produced by subdividing a square
-- into smaller squares
subSquares : Int -> Int -> List (List (Int, Int))
subSquares smallSide bigSide =
    let offsets = unfoldWhile (\x -> x + smallSide) (\x -> x < bigSide) 0
        pairs l = List.map (\x -> (List.map (\y -> (x,y)) l)) l
    in  pairs offsets

---------------------------------------------------------------------

-- Help create colors as Vectors
hslToVec3 : Float -> Float -> Float -> Vec3
hslToVec3 hue saturation lightness =
    let c = toRgb (hsl hue saturation lightness)
    in  vec3 (toFloat c.red / 255) (toFloat c.green / 255) (toFloat c.blue / 255)

