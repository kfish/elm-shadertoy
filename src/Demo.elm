module Demo (demoThings) where

import Maybe (maybe)
import Math.Vector3 (..)

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
import Physics.Collisions (..)
import Behavior.Boids (..)

import Debug (log)

demoThings : Signal [Thing]
demoThings =
    let
        isOdd x = (floor x % 2) == 0
        ifelse cond x y = if cond then x else y
        switchy = isOdd <~ foldp (+) 0 (fps 1)
        cd = extractThing <~ lift3 ifelse (lift fst xvCube) cloudsCube cloudsDiamond

        boids0 = randomBoids 100 (bflys 100 voronoiDistances)
{-
        boids : Signal [Thing]
        boids = map extractThing <~ folds [] moveBoids boids0 (fps 60)
-}
        -- boids = map extractThing <~ runBoids boids0 (fps 60)

        -- boids2TCont = lift3 tcAndThen boids0 (simpleTCont moveBoids <~ boids0) (simpleTCont collisions <~ boids0)
        boids2TCont = tcAndThenSig boids0 (lift boidsTCont) (lift (simpleTCont collisions))

        boids = map extractThing <~ foldSigTCont (boidsTCont []) boids2TCont (fps 60)

        balls0 = randomDrops 15 (spheres 15 fogMountains)

        balls : Signal [Thing]
        balls = map extractThing <~ folds [] moveDrops balls0 (fps 60)

        individuals : Signal [Thing]
        individuals = combine [
            ground,
            -- -- place   0   3   0 <~ teapot,
            place   3   3   1 <~ (extractThing <~ plasmaPortal),
            place   0   1   0 <~ (extractThing <~ fogMountainsSphere),
            place   5 1.5   1 <~ cd,
            place -10   0 -10 <~ (extractThing <~ fireCube),
            -- lift2 (\y e -> place 0 y 0 e) s fireCube,
            place  10 1.5 -10 <~ (extractThing <~ fogMountainsCube)
            ]
    in
        gather [individuals, boids, balls]
