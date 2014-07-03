module Physics.Drop where

import Array
import Math.Vector3 as V3
import Math.Vector3 (Vec3, vec3)
import Math.Matrix4 (..)
import Math.RandomVector (..)

import Engine (..)

import Debug(log)

type Drop =
    { position : Vec3
    , velocity : Vec3
    , orientation : Vec3
    , thing : Thing
    , radius : Float
    }

newDrop : Vec3 -> Vec3 -> Thing -> Drop
newDrop pos vel thing = orientDrop (Drop pos vel (vec3 0 0 0) thing 1.0)

orientDrop : Drop -> Drop
orientDrop d =
    let v = V3.toRecord d.velocity
    in { d | orientation <- V3.normalize (vec3 v.x 0 v.z) }

stepDrop : Time -> Drop -> Drop
stepDrop dt b = { b | position <- b.position `V3.add` (V3.scale (dt / second) b.velocity) }

randomDrop : Signal Thing -> Signal Drop
randomDrop thing =
    let pos = (V3.add (vec3 0 30 0)) <~ randomVec3 4.0
    in
        newDrop <~ pos ~ randomVec3 8.0 ~ thing

randomDrops : Int -> Signal [Thing] -> Signal [Drop]
randomDrops n things =
    let poss = map (V3.add (vec3 0 30 0)) <~ randomVec3s n 4.0
    in
        zipWith3 newDrop <~ poss ~ randomVec3s n 8.0 ~ things

{-
rule1 : Int -> Vec3 -> Drop -> Vec3 
rule1 n sumPos b =
    let perceived_scale = 1.0 / (toFloat (n-1))
        perceived_center = V3.scale perceived_scale (sumPos `V3.sub` b.position)
    in V3.scale (1/25) <| perceived_center `V3.sub` b.position

rule2 : [Vec3] -> Drop -> Vec3
rule2 poss b =
    let f pos = let d = V3.distanceSquared pos b.position
                in if (d > 0 && d < 10.0) then b.position `V3.sub` pos else vec3 0 0 0
    in V3.scale (1/2) <| foldl1 V3.add (map f poss)

rule3 : Int -> Vec3 -> Drop -> Vec3
rule3 n sumVel b =
    let perceived_scale = 1.0 / (toFloat (n-1))
        perceived_vel = V3.scale perceived_scale (sumVel `V3.sub` b.velocity)
    in V3.scale (1/10) <| perceived_vel `V3.sub` b.velocity
-}

-- Run an update function over all combinations of 2 elements of an array
updatePairs : (a -> a -> Maybe (a, a)) -> Array.Array a -> Array.Array a
updatePairs f arr0 =
    let len = Array.length arr0
        -- update m n arr = log ("Updating ("++show m++","++show n++")") <| case f (Array.getOrFail m arr) (Array.getOrFail n arr) of
        update m n arr = case f (Array.getOrFail m arr) (Array.getOrFail n arr) of
            Nothing       -> arr
            Just (vm, vn) -> Array.set m vm . Array.set n vn <| arr
        row m arr = foldl (update m) arr [m+1..len-1]
    in foldl row arr0 [0..len-1]

collide : Time -> Drop -> Drop -> Maybe (Drop, Drop)
collide dt a b =
    let dist = V3.length(V3.sub b.position a.position) - a.radius - b.radius
        mov = V3.scale (dt / second) (V3.sub b.velocity a.velocity)
        standstill d = { d | velocity <- V3.scale 0.95 d.velocity }
    in if | V3.length(mov) < dist -> Just (standstill a, standstill b)
          | otherwise             -> Just (a, b)

collisions : Time -> [Drop] -> [Drop]
collisions dt = Array.toList . updatePairs (collide dt) . Array.fromList

bounds : Drop -> Drop
bounds b =
    let bound vs s low high = let dampVs = -vs * 0.99 in
            if | vs < 0 && s < low  -> dampVs
               | vs > 0 && s > high -> dampVs
               | otherwise          -> vs
        (x,y,z) = V3.toTuple b.position
        (vx,vy,vz) = V3.toTuple b.velocity
    in { b | velocity <- vec3 (bound vx x -20 20) (bound vy y (b.radius) 100) (bound vz z -20 20) }

gravity : Drop -> Vec3
gravity d = vec3 0 -9.8 0

boundVelocity : Vec3 -> Vec3
boundVelocity v = let l = V3.length v in if (l<1) then (V3.scale (1/l) v) else v

moveDrops : Time -> [Drop] -> [Drop]
moveDrops dt boids =
    let
        nboids = length boids
        positions = map .position boids
        velocities = map .velocity boids
        sumPos = foldl1 V3.add positions
        sumVel = foldl1 V3.add velocities
        gs = map gravity boids
        applyRules b g = { b |
            velocity <- (b.velocity `V3.add` (V3.scale (dt / second) g)) }
        bs = zipWith applyRules boids gs
    in map (orientDrop . stepDrop dt) . collisions dt . (map bounds) <| bs
