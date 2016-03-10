module Update (step) where

import Math.Vector3 exposing (..)
import Math.Vector3 as V3
import Math.Matrix4 exposing (..)
import Math.Quaternion as Qn

import Array2D exposing (Array2D)
import Model
import Things.Terrain as Terrain

import Debug

type alias EyeLevel = Vec3 -> Float


step : Array2D Float -> Model.Inputs -> Model.Person -> Model.Person
step terrain inputs person0 = if inputs.reset then Model.defaultPerson else
        let 
            eyeLevel pos = Model.eyeLevel + Terrain.elevation terrain pos
            person = selectVehicle inputs person0
        in
            if person.flying then
                  person |> fly eyeLevel inputs
                         |> gravity eyeLevel inputs.dt
                         |> flyPhysics eyeLevel inputs.dt
            else
                  person |> turn eyeLevel inputs.mx inputs.my
                         |> walk eyeLevel inputs
                         -- |> jump eyeLevel inputs.isJumping
                         |> gravity eyeLevel inputs.dt
                         |> physics eyeLevel inputs.dt

selectVehicle : Model.Inputs -> Model.Person -> Model.Person
selectVehicle inputs person =
    let
        switch = inputs.button_X
        flying = switch `xor` person.flying
    in
        if not switch then
            person
        else if flying then
          Debug.log "Switch to flying!" <|
            { person | flying = True }
        else
          Debug.log "Switch to buggy!" <|
            { person | flying = False
                     , roll = 0
                     , pitch = clamp (degrees -60) (degrees 60) (person.pitch/2)
                     }

flatten : Vec3 -> Vec3
flatten v =
    let r = toRecord v
    in  normalize (vec3 r.x 0 r.z)

turn : EyeLevel -> Float -> Float -> Model.Person -> Model.Person
turn eyeLevel dx dy person =
    let
        mx = if getY person.pos > (eyeLevel person.pos) + 5 then 10.0 else 1.0
        h' = person.yaw - (dx * mx)
        v' = person.pitch + dy
    in
        { person | yaw = h'
                 , pitch = clamp (degrees -60) (degrees 60) v'
                 -- , pitch = v'
        }

-- http://www.dtic.mil/dtic/tr/fulltext/u2/a152616.pdf
fly : EyeLevel -> Model.Inputs -> Model.Person -> Model.Person
fly eyeLevel inputs person =
    let
        thrust = inputs.y

        yaw'   = person.yaw   - 5 * inputs.x  * inputs.dt
        pitch' = person.pitch + 4 * inputs.my * inputs.dt
        roll'  = person.roll  + 6 * inputs.mx * inputs.dt

        orient = Qn.vrotate (Qn.fromEuler roll' pitch' yaw')
        dv = V3.scale (30 * thrust * inputs.dt) <| orient V3.k
        du = V3.scale (20 * thrust * inputs.dt) <| orient V3.j
        dv' = dv `add` du

        vel = (V3.scale 0.8 dv') `add` (V3.scale 0.95 person.velocity)
        
    in
        { person | yaw = yaw'
                 , pitch = pitch'
                 , roll = roll'
                 , velocity = vel
        }

walk : EyeLevel -> { a | x:Float, y:Float, dt:Float } -> Model.Person -> Model.Person
walk eyeLevel directions person =
  -- if getY person.pos > eyeLevel person.pos then person else
    let moveDir = normalize (flatten (Model.direction person))
        strafeDir = transform (makeRotate (degrees -90) j) moveDir

        move = V3.scale (8.0 * directions.y) moveDir
        strafe = V3.scale (8.0 * directions.x) strafeDir

        e = (eyeLevel person.pos) / 80.0
        -- friction = 0.5
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

v3_clamp : Float -> Vec3 -> Vec3
v3_clamp len v = if V3.length v <= len then v else V3.scale len (V3.normalize v)

adjustVelocity : Float -> Float -> Vec3 -> Float -> Vec3 -> Vec3
adjustVelocity maxSpeed friction dv dt v =
    v3_clamp maxSpeed <| add (V3.scale dt dv) (V3.scale (1.0-(friction*dt)) v)

{-
jump : EyeLevel -> Bool -> Model.Person -> Model.Person
jump eyeLevel isJumping person =
  -- if not isJumping || getY person.pos > eyeLevel person.pos then person else
  if not isJumping then person else
    let v = toRecord person.velocity
    in
        { person | velocity = vec3 v.x (1.0*80) v.z }
-}

flyPhysics : EyeLevel -> Float -> Model.Person -> Model.Person
flyPhysics eyeLevel dt person =
    let pos = Terrain.bounds <| person.pos `add` V3.scale dt person.velocity
        p = toRecord pos
        e = eyeLevel pos

        (pos', dv) = if p.y < e then
                         (vec3 p.x e p.z, vec3 0 0 0)
                     else
                         (pos, vec3 0 0 0)
    in
        { person | pos = pos', velocity = person.velocity `add` dv }

physics : EyeLevel -> Float -> Model.Person -> Model.Person
physics eyeLevel dt person =
    let pos = Terrain.bounds <| person.pos `add` V3.scale dt person.velocity
        p = toRecord pos
        e = eyeLevel pos
        vy0 = getY person.velocity

        (pos', dv) = if p.y < e then
                         let vy = if ((e < (0.8*80) && vy0 > -30) || vy0 > -9.8) && e - p.y > (10*dt) then
                             clamp 0 40 (V3.length person.velocity * (e-p.y)*dt*10) else 0
                         in (vec3 p.x e p.z, vec3 0 vy 0)
                     else
                         (pos, vec3 0 0 0)
    in
        { person | pos = pos', velocity = person.velocity `add` dv }

gravity : EyeLevel -> Float -> Model.Person -> Model.Person
gravity eyeLevel dt person =
  if getY person.pos <= eyeLevel person.pos then person else
    let v = toRecord person.velocity
    in
        { person | velocity = vec3 v.x (v.y - 9.8 * dt) v.z }
