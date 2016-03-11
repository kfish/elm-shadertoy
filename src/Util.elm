module Util where

import List.Extra exposing (splitAt)

import Color exposing (hsl, toRgb)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector3 as V3

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
    let offsets = unfoldWhile (\x -> x + smallSide) (\x -> (x+smallSide) <= bigSide) 0
        pairs l = List.map (\x -> (List.map (\y -> (x,y)) l)) l
    in  pairs offsets

-- Integer coordinates within radius r of the origin
iradius : Int -> List (Int, Int)
iradius r =
    let
        l0 = unfoldWhile (\x -> x+1) (\x -> x <= r) 1
        l y0 = unfoldWhile (\(x,y) -> (x+1,y)) (\(x,y) -> (x*x) + (y*y) <= r*r) (0,y0)
        rots (x,y) = [(x,y), (-y,x), (-x,-y), (y, -x)]
    in
        (0,0) :: (List.concatMap rots <| List.concat <| List.map l l0)

---------------------------------------------------------------------

-- Help create colors as Vectors
hslToVec3 : Float -> Float -> Float -> Vec3
hslToVec3 hue saturation lightness =
    let c = toRgb (hsl hue saturation lightness)
    in  vec3 (toFloat c.red / 255) (toFloat c.green / 255) (toFloat c.blue / 255)

-- | Clamp a vector to be no longer than len
v3_clamp : Float -> Vec3 -> Vec3
v3_clamp len v = if V3.length v <= len then v else V3.scale len (V3.normalize v)

