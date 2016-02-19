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

