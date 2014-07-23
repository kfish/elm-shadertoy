module Demo (demoThings) where

import Math.Vector3 (..)
import Math.Matrix4 (..)

import Engine (..)

import Things.Ground (ground)
import Things.BFly (bflys, fireBFly, voronoiBFly, voronoiBFlys)
import Things.Cube (cloudsCube, fireCube, fogMountainsCube, plasmaCube, voronoiCube, xvCube)
import Things.Diamond (cloudsDiamond, fogMountainsDiamond)
import Things.Portal (plasmaPortal)
import Things.Sphere (spheres, cloudsSphere, fogMountainsSphere)
import Things.Teapot (teapot)
import Shaders.FogMountains (fogMountains)
import Shaders.VoronoiDistances (voronoiDistances)

import Physics.Drop (..)
import Behavior.Boids (..)

import Debug (log)

folds : b -> (a -> b -> b) -> Signal b -> Signal a -> Signal b
folds dfl step state input =
    let f g (b0,is) bm = case bm of
            Nothing -> Just b0
            Just b -> Just (g is b)
    in maybe dfl id <~ foldp (f step) Nothing (lift2 (,) state input)

demoThings : Signal [Thing]
demoThings =
    let
        isOdd x = (floor x `mod` 2) == 0
        ifelse cond x y = if cond then x else y
        switchy = isOdd <~ foldp (+) 0 (fps 1)
        cd = extractThing <~ lift3 ifelse (lift fst xvCube) cloudsCube cloudsDiamond

        sinLog x = sin (x/1000)
        sinFoo = foldp (+) 0 (fps 60)
        s = sinLog <~ sinFoo

        boid0 : Signal (Boid {})
        boid0 = randomBoid plasmaCube

        boids0 : Signal [Boid {}]
        boids0 = randomBoids 100 (bflys 100 voronoiDistances)

        boids : Signal [Thing]
        -- boids = map orient <~ folds [] moveBoids boids0 (fps 60)
        boids = map extractThing <~ folds [] moveBoids boids0 (fps 60)

        -- balls0 : Signal [Drop]
        balls0 = randomDrops 15 (spheres 15 fogMountains)

        balls : Signal [Thing]
        -- balls = map orient <~ folds [] moveDrops balls0 (fps 60)
        balls = map extractThing <~ folds [] moveDrops balls0 (fps 60)

        individuals : Signal [Thing]
        individuals = combine [
            ground,
            -- place   0   3   0 <~ teapot,
            place   3   3   1 <~ plasmaPortal,
            place   0   1   0 <~ fogMountainsSphere,
            place   5 1.5   1 <~ cd,
            lift2 (\x e -> tview (rotate (x/1000) (vec3 3 1 5)) . place  10   0  10 <| e) sinFoo (lift snd xvCube),
            -- place -10   0 -10 <~ fireCube,
            -- lift2 (\y e -> place 0 y 0 e) s fireCube,
            place  10 1.5 -10 <~ fogMountainsCube
            ]
    in
        gather [individuals, boids, balls]
