module Behavior.Boids where

import Random (..)

import Math.Vector3 as V3
import Math.Vector3 (Vec3, vec3)
import Math.Matrix4 (..)

import Engine (..)

type Boid =
    { position : Vec3
    , velocity : Vec3
    , thing : Thing
    }

newBoid : Vec3 -> Vec3 -> Thing -> Boid
newBoid = Boid

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
    let fromUV [u,v] =
            let theta = 2 * pi * u
                phi = acos (2 * v - 1)
            in fromSpherical r theta phi
    in fromUV <~ floatList (constant 2)

randomUnitVec3 : Signal Vec3
randomUnitVec3 = randomVec3 1

{- Generate n random vectors with given length
   http://mathworld.wolfram.com/SpherePointPicking.html
-}
randomVec3s : Int -> Float -> Signal [Vec3]
randomVec3s n r =
    let fromUV [u,v] =
            let theta = 2 * pi * u
                phi = acos (2 * v - 1)
            in fromSpherical r theta phi
        pairs xs0 = case xs0 of
            []         -> []
            [x]        -> []
            x1::x2::xs -> [x1,x2] :: pairs xs
    in map fromUV . pairs <~ floatList (constant (2*n))

orient : { r | thing:Thing, position:Vec3, velocity:Vec3 } -> Thing
orient o =
    let
        v = V3.toRecord o.velocity
        dir = V3.normalize (vec3 v.x (v.y/10) v.z)
        z_axis = vec3 0 0 1
        rot_angle = 0 - acos (V3.dot dir z_axis)
        rot_axis = V3.normalize (V3.cross dir z_axis)
    in
        tview (translate o.position) . tview (rotate rot_angle rot_axis) <| o.thing

stepBoid : Time -> Boid -> Boid
stepBoid dt b = { b | position <- b.position `V3.add` (V3.scale (dt / second) b.velocity) }

randomBoid : Signal Thing -> Signal Boid
randomBoid thing =
    let pos = (V3.add (vec3 7 8 4)) <~ randomVec3 4.0
    in
        newBoid <~ pos ~ randomVec3 1.0 ~ thing

randomBoids : Int -> Signal [Thing] -> Signal [Boid]
randomBoids n things =
    let poss = map (V3.add (vec3 7 8 4)) <~ randomVec3s n 4.0
    in
        zipWith3 newBoid <~ poss ~ randomVec3s n 1.0 ~ things

rule1 : Int -> Vec3 -> Boid -> Vec3 
rule1 n sumPos b =
    let perceived_scale = 1.0 / (toFloat (n-1))
        perceived_center = V3.scale perceived_scale (sumPos `V3.sub` b.position)
    in V3.scale (1/25) <| perceived_center `V3.sub` b.position

rule2 : [Vec3] -> Boid -> Vec3
rule2 poss b =
    let f pos = let d = V3.distanceSquared pos b.position
                in if (d > 0 && d < 10.0) then b.position `V3.sub` pos else vec3 0 0 0
    in V3.scale (1/2) <| foldl1 V3.add (map f poss)

rule3 : Int -> Vec3 -> Boid -> Vec3
rule3 n sumVel b =
    let perceived_scale = 1.0 / (toFloat (n-1))
        perceived_vel = V3.scale perceived_scale (sumVel `V3.sub` b.velocity)
    in V3.scale (1/10) <| perceived_vel `V3.sub` b.velocity

bounds : Boid -> Vec3
bounds b =
    let bound x low high = if (x < low) then 1 else (if x > high then -1 else 0)
        (x,y,z) = V3.toTuple b.position
    in vec3 (bound x -20 20) (bound y 0 30) (bound z -20 20)

boundVelocity : Vec3 -> Vec3
boundVelocity v = let l = V3.length v in if (l<1) then (V3.scale (1/l) v) else v

{-
randomBoids : Int -> Signal Thing -> Signal [Boid]
randomBoids n0 thing =
    let f n = if (n==0) then [] else randomBoid thing :: f (n-1)
    in combine <| f n0
-}
    

moveBoids : Time -> [Boid] -> [Boid]
moveBoids dt boids =
    let
        nboids = length boids
        positions = map .position boids
        velocities = map .velocity boids
        sumPos = foldl1 V3.add positions
        sumVel = foldl1 V3.add velocities
        r1s = map (rule1 nboids sumPos) boids
        r2s = map (rule2 positions) boids
        r3s = map (rule3 nboids sumVel) boids
        box = map bounds boids
        applyRules b r1 r2 r3 r4 = { b |
            velocity <- boundVelocity (b.velocity `V3.add` (V3.scale (dt / second)
                (r1 `V3.add` r2 `V3.add` r3 `V3.add` r4))) }
        bs = zipWith5 applyRules boids r1s r2s r3s box
    in map (stepBoid dt) bs
