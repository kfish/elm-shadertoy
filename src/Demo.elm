module Demo (demoThings) where

import Math.Vector3 exposing (..)
import Random
import Signal.Extra exposing ((<~), combine)
import Time exposing (fps)

import Math.RandomVector exposing (randomVec3')

import Engine exposing (..)

import Things.Ground exposing (ground)
import Things.BFly exposing (bfly)
import Things.Cube exposing (cloudsCube, fireCube, fogMountainsCube, plasmaCube, voronoiCube, xvCube)
import Things.Diamond exposing (cloudsDiamond, fogMountainsDiamond)
import Things.Portal exposing (plasmaPortal)
import Things.Sphere exposing (spheres, cloudsSphere, fogMountainsSphere)
-- import Things.Teapot exposing (teapot)
import Shaders.FogMountains exposing (fogMountains)
import Shaders.VoronoiDistances exposing (voronoiDistances)

import Physics.Drop exposing (..)
import Physics.Collisions exposing (..)
import Behavior.Boids exposing (..)

import Debug exposing (log)

randomBFly : Random.Generator (Visible (Oriented {}))
randomBFly = Random.map (bfly voronoiDistances) (Random.float 0.0 1.0)

randomBoid : Random.Generator (Boid (Visible {}))
randomBoid = Random.map3
    (newBoid 0.3 1.0)
    (Random.map (add (vec3 7 8 4)) (randomVec3' 4.0))
    (randomVec3' 1.0)
    randomBFly

randomDrop : Random.Generator (Drop (Visible {}))
randomDrop = Random.map2
    (\pos vel -> newDrop pos vel fogMountainsSphere)
    (Random.map (add (vec3 0 30 0)) (randomVec3' 4.0))
    (randomVec3' 8.0)

demoThings : Signal (List Thing)
demoThings =
    let
        isOdd x = (floor x % 2) == 0
        ifelse cond x y = if cond then x else y
        switchy = isOdd <~ Signal.foldp (+) 0 (fps 1)
        cd = extractThing <~ Signal.map3 ifelse (Signal.map fst xvCube) cloudsCube cloudsDiamond

        seed0 = Random.initialSeed 7

{-
        (rands, seed1) = Random.generate (Random.list 100 (Random.float 0.0 1.0)) seed0

        boids0 = randomBoids 100 (List.map (bfly voronoiDistances) rands)
-}

        (boids0, seed1) = Random.generate (Random.list 100 randomBoid) seed0
{-
        boids : Signal [Thing]
        boids = map extractThing <~ folds [] moveBoids boids0 (fps 60)
-}

        boids = List.map extractThing <~ runBoids boids0 (fps 60)

        -- -- boidsColl = tcAndThen boidsTCont (simpleTCont collisions)
        -- boidsColl = composeTCont moveBoids collisions
        -- boids = map extractThing <~ foldSigTCont2 [] boidsColl boids0 (fps 60)

        (balls0, seed2) = Random.generate (Random.list 15 randomDrop) seed1

        balls : Signal (List Thing)
        -- balls = List.map extractThing <~ folds [] moveDrops balls0 (fps 60)
        balls = List.map extractThing <~ Signal.foldp moveDrops balls0 (fps 60)

        individuals : Signal (List Thing)
        individuals = combine [
            ground,
            -- -- place   0   3   0 <~ teapot,
            place   3   3   1 <~ (extractThing <~ plasmaPortal),
            place   0   1   0 <~ (extractThing <~ Signal.constant fogMountainsSphere),
            place   5 1.5   1 <~ cd,
            place -10   0 -10 <~ (extractThing <~ fireCube),
            -- lift2 (\y e -> place 0 y 0 e) s fireCube,
            place  10 1.5 -10 <~ (extractThing <~ fogMountainsCube)
            ]
    in
        gather [individuals, boids, balls]
