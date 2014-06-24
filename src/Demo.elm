module Demo (demoThings) where

import Math.Vector3 (..)
import Math.Matrix4 (..)

import Engine (..)

import Things.Ground (ground)
import Things.Cube (cloudsCube, fireCube, fogMountainsCube, plasmaCube, voronoiCube, xvCube)
import Things.Diamond (cloudsDiamond, fogMountainsDiamond)
import Things.Teapot (teapot)

import Debug (log)

demoThings : Signal Things
demoThings =
    let
        isOdd x = log (show x) ((floor x `mod` 2) == 0)
        ifelse cond x y = if cond then x else y
        switchy = isOdd <~ foldp (+) 0 (fps 1)
        cd = lift3 ifelse (lift fst xvCube) cloudsCube cloudsDiamond

        sinLog x = log (show x) (sin (x/1000))
        sinFoo = foldp (+) 0 (fps 60)
        s = sinLog <~ sinFoo
    in
        gather [
            ground,
            -- place   0   3   0 <~ teapot,
            place   5 1.5   1 <~ cd,
            lift2 (\x e -> tview (rotate (x/1000) (vec3 3 1 5)) . place  10   0  10 <| e) sinFoo (lift snd xvCube),
            -- place -10   0 -10 <~ fireCube,
            lift2 (\y e -> place 0 y 0 e) s fireCube,
            place  10 1.5 -10 <~ fogMountainsCube
            ]
