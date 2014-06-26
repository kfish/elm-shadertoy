module Behavior.Boids where

import Random (..)

import Math.Vector3 (..)
import Math.Vector3 as V3
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

randomBoid : Signal Thing -> Signal Boid
randomBoid thing =
    let pos = (add (vec3 7 8 4)) <~ randomVec3 4.0
    in
        newBoid <~ pos ~ randomUnitVec3 ~ thing

boidThing : Boid -> Thing
boidThing b = tview (translate b.position) b.thing

moveBoid : Time -> Boid -> Boid
moveBoid dt b = { b | position <- b.position `add` (V3.scale (dt / second) b.velocity) }

