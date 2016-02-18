module Array2D where

import Array

type alias Array2D a = Array.Array a

side : Array2D a -> Int
side arr0 = round <| sqrt <| toFloat <| Array.length arr0

-- This will fail for non-square Arrays (getXY will return def, setXY will noop)
xy : Array2D a -> Int -> Int -> Int
xy arr x y = x + y * (round <| sqrt <| toFloat <| Array.length arr)

getXY : Int -> Int -> a -> Array2D a -> a
getXY x y def arr = Maybe.withDefault def (Array.get (xy arr x y) arr)

setXY : Int -> Int -> a -> Array2D a -> Array2D a
setXY x y val arr = Array.set (xy arr x y) val arr

