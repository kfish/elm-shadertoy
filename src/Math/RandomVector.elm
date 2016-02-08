module Math.RandomVector where

import Random exposing (..)
import Signal

import Math.Vector3 exposing (Vec3, vec3)

{-
http://mathworld.wolfram.com/SphericalCoordinates.html
-}
fromSpherical : Float -> Float -> Float -> Vec3
fromSpherical r theta phi =
    let x = r * cos theta * sin phi
        y = r * sin theta * sin phi
        z = r * cos theta
    in vec3 x y z

{- Generate a random vector with given length
   http://mathworld.wolfram.com/SpherePointPicking.html
-}
randomVec3 : Float -> Signal Vec3
randomVec3 r =
    let fromUV (u,v) =
            let theta = 2 * pi * u
                phi = acos (2 * v - 1)
            in fromSpherical r theta phi
    -- in Signal.map fromUV (floatList (Signal.constant 2))
    -- in Signal.map fromUV (list 2 (float 0 1))
    in Signal.constant (vec3 0 0 0)

randomUnitVec3 : Signal Vec3
randomUnitVec3 = randomVec3 1

{- Generate n random vectors with given length
   http://mathworld.wolfram.com/SpherePointPicking.html
-}
randomVec3s : Int -> Float -> Signal (List Vec3)
randomVec3s n r =
    let fromUV (u,v) =
            let theta = 2 * pi * u
                phi = acos (2 * v - 1)
            in fromSpherical r theta phi
        pairs xs0 = case xs0 of
            []         -> []
            [x]        -> []
            x1::x2::xs -> (x1,x2) :: pairs xs
    -- in map fromUV << Signal.map pairs (floatList (Signal.constant (2*n)))
    -- in map fromUV << Signal.map pairs (list (2*n) (float 0 1))
    in Signal.constant []

