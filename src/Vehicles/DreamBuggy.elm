module Vehicles.DreamBuggy (move, welcome) where

import Math.Vector3 exposing (..)
import Math.Vector3 as V3
import Math.Matrix4 exposing (..)
import Math.Quaternion as Qn
import Util exposing (v3_clamp)

import Array2D exposing (Array2D)
import Model

import Debug

----------------------------------------------------------------------
-- DreamBuggy

move : Model.EyeLevel -> Model.Inputs -> Model.Person -> Model.Person
move eyeLevel inputs person =
    person |> turn eyeLevel inputs.mx inputs.my
           |> walk eyeLevel inputs
           -- |> jump eyeLevel inputs.isJumping
           |> physics eyeLevel inputs.dt

-- | Welcome a new driver to the DreamBuggy
welcome : Model.Person -> Model.Person
welcome person = { person | orientQn = clampBuggy person.orientQn }

clampBuggy : Qn.Quaternion -> Qn.Quaternion
clampBuggy q =
    let (roll, pitch, yaw) = Qn.toEuler q
        roll' = clamp (degrees -30) (degrees 30) (roll/2)
        pitch' = clamp (degrees -60) (degrees 60) (pitch/2)
    in Qn.fromEuler (roll', pitch', yaw)

flatten : Vec3 -> Vec3
flatten v =
    let r = toRecord v
    in  normalize (vec3 r.x 0 r.z)

turn : Model.EyeLevel -> Float -> Float -> Model.Person -> Model.Person
turn eyeLevel dx dy person =
    let
        (roll0, pitch0, yaw0) = Qn.toEuler person.orientQn
        personY = getY person.pos
        frontTireY = eyeLevel (person.pos `add` (Qn.vrotate person.orientQn (vec3 0 0 0.1)))
        rightTireY = eyeLevel (person.pos `add` (Qn.vrotate person.orientQn (vec3 0.1 0 0)))
        leftTireY = eyeLevel (person.pos `add` (Qn.vrotate person.orientQn (vec3 -0.1 0 0)))
        tirePitch = atan ((frontTireY - personY)/0.1)
        tireRoll  = atan (-(rightTireY - leftTireY)/0.1)
        (yaw, pitch, roll) =
            if getY person.pos > (eyeLevel person.pos) + 5 then
                (yaw0-(dx * 5), pitch0*0.9 + dy*0.1, 0)
            else
                (yaw0-dx, pitch0*0.95 + (tirePitch+dy)*0.05, roll0*0.95 + (tireRoll*0.05))

        orientQn = clampBuggy (Qn.fromEuler (roll, pitch, yaw))
    in
        { person | orientQn = orientQn }

walk : Model.EyeLevel -> { a | x:Float, y:Float, dt:Float } -> Model.Person -> Model.Person
walk eyeLevel directions person =
  -- if getY person.pos > eyeLevel person.pos then person else
    let moveDir = normalize (flatten (Model.direction person))
        strafeDir = transform (makeRotate (degrees -90) j) moveDir

        move = V3.scale (8.0 * directions.y) moveDir
        strafe = V3.scale (8.0 * directions.x) strafeDir

        e = (eyeLevel person.pos) / 80.0

        friction = if e > 0.8 then -0.1
                   else if e < 0.0 then 0.8
                   else if e < 0.1 then 0.6
                   else if e < 0.15 then 0.5
                   else 0.2

        maxSpeed = if e > 0.8 then 30
                   else if e < 0.0 then 3
                   else if e < 0.1 then 10
                   else if e < 0.15 then 15
                   else 20
    in
        { person | velocity = adjustVelocity maxSpeed friction (move `add` strafe) directions.dt person.velocity }

adjustVelocity : Float -> Float -> Vec3 -> Float -> Vec3 -> Vec3
adjustVelocity maxSpeed friction dv dt v =
    v3_clamp maxSpeed <| add (V3.scale dt dv) (V3.scale (1.0-(friction*dt)) v)

{-
jump : Model.EyeLevel -> Bool -> Model.Person -> Model.Person
jump eyeLevel isJumping person =
  -- if not isJumping || getY person.pos > eyeLevel person.pos then person else
  if not isJumping then person else
    let v = toRecord person.velocity
    in
        { person | velocity = vec3 v.x (1.0*80) v.z }
-}

physics : Model.EyeLevel -> Float -> Model.Person -> Model.Person
physics eyeLevel dt person =
    let pos = person.pos `add` V3.scale dt person.velocity
        p = toRecord pos
        e = eyeLevel pos
        vy0 = getY person.velocity

        (pos', dv) = if p.y < e then
                         let vy = if ((e < (0.8*80) && vy0 > -30) || vy0 > -9.8) && e - p.y > (10*dt) then
                             clamp 0 40 (V3.length person.velocity * (e-p.y)*dt*5) else 0
                         in (vec3 p.x e p.z, vec3 0 vy 0)
                     else
                         (pos, vec3 0 0 0)
    in
        { person | pos = pos', velocity = person.velocity `add` dv }
