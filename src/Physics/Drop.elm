module Physics.Drop where

import Array
import Math.Vector3 as V3
import Math.Vector3 (Vec3, vec3)
import Math.Matrix4 (..)
import Math.RandomVector (..)

import Engine (..)

import Physics.Collisions (..)

import Debug(log)

-- newDrop : Vec3 -> Vec3 -> a -> Drop a
newDrop pos vel thing0 =
    let thing1= { thing0 | position <- pos }
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
        newDrop <~ pos ~ randomVec3 8.0 ~ thing

-- randomDrops : Int -> Signal [a] -> Signal [Drop a]
randomDrops n things =
    let poss = map (V3.add (vec3 0 30 0)) <~ randomVec3s n 4.0
    in
        zipWith3 newDrop <~ poss ~ randomVec3s n 8.0 ~ things

bounds : BBall a -> BBall a
bounds b =
    let bound vs s low high = let dampVs = -vs * 0.99 in
            if | vs < 0 && s < low  -> dampVs
               | vs > 0 && s > high -> dampVs
               | otherwise          -> vs
        (x,y,z) = V3.toTuple b.position
        (vx,vy,vz) = V3.toTuple b.velocity
    in { b | velocity <- vec3 (bound vx x -20 20) (bound vy y (b.radius) 100) (bound vz z -20 20) }

gravity : a -> Vec3
gravity _ = vec3 0 -9.8 0

moveDrops : Time -> [BBall a] -> [BBall a]
moveDrops dt balls =
    let
        gs = map gravity balls
        applyRules b g = { b |
            velocity <- (b.velocity `V3.add` (V3.scale (dt / second) g)) }
        bs = map bounds <| zipWith applyRules balls gs
    in collisions dt <| bs
