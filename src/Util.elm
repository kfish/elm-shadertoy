module Util where

import List.Extra exposing (splitAt)

splitEvery : Int -> List a -> List (List a)
splitEvery size xs = case xs of
    [] -> []
    _  -> let (p,q) = splitAt size xs in p :: splitEvery size q

bammel : Int -> (a -> a) -> a -> a
bammel n f x = if n <= 0 then x else bammel (n-1) f (f x)

unfoldWhile : (a -> a) -> (a -> Bool) -> a -> List a
unfoldWhile f p x = if p x then x :: unfoldWhile f p (f x) else []
