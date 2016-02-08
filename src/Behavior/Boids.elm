module Behavior.Boids where

import Math.Vector3 as V3
import Math.Vector3 exposing (Vec3, vec3)
import Math.Matrix4 exposing (..)
import Math.RandomVector exposing (..)
import Signal.Extra exposing ((<~))
import Time exposing (Time, second)

import Engine exposing (..)

type alias Boid a = Massive (Spherical (Moving a))

-- newBoid : Float -> Float -> Vec3 -> Vec3 -> a -> Boid a
newBoid m r pos vel thing0 =
    let thing1 = { thing0 | radius = r } -- add field
        thing2 = { thing1 | mass = m } -- add field
        thing3 = { thing2 | pos = pos } -- update field
        thing4 = { thing3 | velocity = vel } -- add field
    in { thing4 | orientation = boidOrientation }

boidOrientation b =
    let v = V3.toRecord b.velocity
    in V3.normalize (vec3 v.x (v.y/10) v.z)

-- stepBoid : Time -> Moving a -> Moving a
stepBoid dt b = { b | pos = b.pos `V3.add` (V3.scale (dt / second) b.velocity) }

-- randomBoid : Signal Thing -> Signal Boid
randomBoid thing =
    let pos = Signal.map2 V3.add (Signal.map .pos thing) (randomVec3 4.0)
    in
        Signal.map3 (newBoid 0.3 0.50) pos (randomVec3 1.0) thing

-- randomBoids : Int -> Signal [Thing] -> Signal [Boid a]
randomBoids n things =
    let poss = List.map (V3.add (vec3 7 8 4)) <~ randomVec3s n 4.0
    in
        Signal.map3 (newBoid 0.3 1.0) poss (randomVec3s n 1.0) things

rule1 : Int -> Vec3 -> Boid a -> Vec3 
rule1 n sumPos b =
    let perceived_scale = 1.0 / (toFloat (n-1))
        perceived_center = V3.scale perceived_scale (sumPos `V3.sub` b.pos)
    in V3.scale (1/25) <| perceived_center `V3.sub` b.pos

rule2 : List Vec3 -> Boid a -> Vec3
rule2 poss b =
    let f pos = let d = V3.distanceSquared pos b.pos
                in if (d > 0 && d < 10.0) then b.pos `V3.sub` pos else vec3 0 0 0
    in V3.scale (1/2) <| sumVec3s (List.map f poss)

rule3 : Int -> Vec3 -> Boid a -> Vec3
rule3 n sumVel b =
    let perceived_scale = 1.0 / (toFloat (n-1))
        perceived_vel = V3.scale perceived_scale (sumVel `V3.sub` b.velocity)
    in V3.scale (1/10) <| perceived_vel `V3.sub` b.velocity

bounds : Boid a -> Vec3
bounds b =
    let bound x low high = if (x < low) then 1 else (if x > high then -1 else 0)
        (x,y,z) = V3.toTuple b.pos
    in vec3 (bound x -20 20) (bound y 0 30) (bound z -20 20)

boundVelocity : Vec3 -> Vec3
boundVelocity v = let l = V3.length v in if (l<1) then (V3.scale (1/l) v) else v

{-
randomBoids : Int -> Signal Thing -> Signal [Boid]
randomBoids n0 thing =
    let f n = if (n==0) then [] else randomBoid thing :: f (n-1)
    in combine <| f n0
-}
    

moveBoids : Time -> List (Boid a) -> List (Boid a)
moveBoids dt boids =
    let
        nboids = List.length boids
        positions = List.map .pos boids
        velocities = List.map .velocity boids
        sumPos = sumVec3s positions
        sumVel = sumVec3s velocities
        r1s = List.map (rule1 nboids sumPos) boids
        r2s = List.map (rule2 positions) boids
        r3s = List.map (rule3 nboids sumVel) boids
        box = List.map bounds boids
        applyRules b r1 r2 r3 r4 = { b |
            velocity = boundVelocity (b.velocity `V3.add` (V3.scale (dt / second)
                (r1 `V3.add` r2 `V3.add` r3 `V3.add` r4))) }
        bs = List.map5 applyRules boids r1s r2s r3s box
    in List.map (stepBoid dt) bs


boidsTCont : TCont (List (Boid a))
boidsTCont = simpleTCont moveBoids

-- runBoids : Signal [a] -> Signal Time -> Signal (TCont [Boid a])
runBoids : Signal (List (Boid a)) -> Signal Time -> Signal (List (Boid a))
runBoids boids0 t = foldSigTCont2 [] boidsTCont boids0 t

{-
runBoids : Signal [Boid a] -> Signal Time -> Signal [Boid a]
runBoids 
-}

sumVec3s : List Vec3 -> Vec3
sumVec3s = List.foldl V3.add (vec3 0 0 0)
