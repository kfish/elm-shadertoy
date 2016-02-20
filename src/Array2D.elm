module Array2D where

import Array
import Util exposing (splitEvery)

type Array2D a = Array2D Int (Array.Array a)

fromArray : Array.Array a -> Array2D a
fromArray arr = let s = round <| sqrt <| toFloat <| Array.length arr in Array2D s arr

toLists : Array2D a -> List (List a)
toLists (Array2D s arr) = splitEvery s (Array.toList arr)

getXY : Int -> Int -> a -> Array2D a -> a
getXY x y def (Array2D s arr) = Maybe.withDefault def (Array.get (x + y*s) arr)

setXY : Int -> Int -> a -> Array2D a -> Array2D a
setXY x y val (Array2D s arr) = Array2D s (Array.set (x + y*s) val arr)

