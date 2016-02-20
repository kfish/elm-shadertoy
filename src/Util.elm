module Util where

import List.Extra exposing (splitAt)

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
