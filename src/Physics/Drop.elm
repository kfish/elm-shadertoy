module Physics.Drop where

import Array
import Math.Vector3 as V3
import Math.Vector3 exposing (Vec3, vec3)
import Math.Matrix4 exposing (..)
import Math.RandomVector exposing (..)
import Signal.Extra exposing ((<~))
import Time exposing (Time, second)

import Engine exposing (..)

import Physics.Collisions exposing (..)

import Debug exposing (log)

-- newDrop : Vec3 -> Vec3 -> a -> Drop a
newDrop pos vel thing0 =
    let thing1 = { thing0 | pos = pos } -- update
        thing2 = { thing1 | velocity = vel }
        thing3 = { thing2 | radius = 1.0 }
        thing4 = { thing3 | mass = 1.0 }
    in { thing4 | orientation = dropOrientation }

dropOrientation d =
    let v = V3.toRecord d.velocity
    in V3.normalize (vec3 v.x 0 v.z)

-- randomDrop : Signal a -> Signal (Drop a)
randomDrop thing =
    let pos = (V3.add (vec3 0 30 0)) <~ randomVec3 4.0
    in
        Signal.map3 newDrop pos (randomVec3 8.0) thing

-- randomDrops : Int -> Signal [a] -> Signal [Drop a]
randomDrops n things =
    let poss = (List.map (V3.add (vec3 0 30 0))) <~ randomVec3s n 4.0
    in
        Signal.map3 (List.map3 newDrop) poss (randomVec3s n 8.0) things

bounds : BBall a -> BBall a
bounds b =
    let bound vs s low high = let dampVs = -vs * 0.99 in
            if vs < 0 && s < low then
                dampVs
            else if vs > 0 && s > high then
                dampVs
            else
                vs
        (x,y,z) = V3.toTuple b.pos
        (vx,vy,vz) = V3.toTuple b.velocity
    in { b | velocity = vec3 (bound vx x -20 20) (bound vy y (b.radius) 100) (bound vz z -20 20) }

gravity : a -> Vec3
gravity _ = vec3 0 -9.8 0

-- moveDrops : Time -> List (BBall a) -> List (BBall a)
moveDrops dt balls =
    let
        gs = List.map gravity balls
        applyRules b g = { b |
            velocity = (b.velocity `V3.add` (V3.scale (dt / second) g)) }
        bs = List.map bounds <| List.map2 applyRules balls gs
    in collisions dt <| bs
