module Demo (demoThings) where

import Math.Vector3 exposing (..)
import Signal.Extra exposing ((<~), combine)
import Time exposing (fps)

import Engine exposing (..)

import Things.Ground exposing (ground)
import Things.BFly exposing (bflys)
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

demoThings : Signal (List Thing)
demoThings =
    let
        isOdd x = (floor x % 2) == 0
        ifelse cond x y = if cond then x else y
        switchy = isOdd <~ Signal.foldp (+) 0 (fps 1)
        cd = extractThing <~ Signal.map3 ifelse (Signal.map fst xvCube) cloudsCube cloudsDiamond

        boids0 = randomBoids 100 (bflys 100 voronoiDistances)
{-
        boids : Signal [Thing]
        boids = map extractThing <~ folds [] moveBoids boids0 (fps 60)
-}

        boids = List.map extractThing <~ runBoids boids0 (fps 60)

        -- -- boidsColl = tcAndThen boidsTCont (simpleTCont collisions)
        -- boidsColl = composeTCont moveBoids collisions
        -- boids = map extractThing <~ foldSigTCont2 [] boidsColl boids0 (fps 60)

        balls0 = randomDrops 15 (spheres 15 fogMountains)

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
