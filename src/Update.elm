module Update (step) where

import Math.Vector3 exposing (..)
import Math.Vector3 as V3
import Math.Matrix4 exposing (..)

import Array2D exposing (Array2D)
import Model
import Things.Terrain as Terrain

type alias EyeLevel = Vec3 -> Float

step : Array2D Float -> Model.Inputs -> Model.Person -> Model.Person
step terrain inputs person =
        let 
            eyeLevel pos = Model.eyeLevel + Terrain.elevation terrain pos
        in
{-
          person |> fly directions
                 |> physics eyeLevel dt
-}
          person |> turn eyeLevel inputs.mx inputs.my
                 |> walk eyeLevel inputs
                 -- |> jump eyeLevel inputs.isJumping
                 |> gravity eyeLevel inputs.dt
                 |> physics eyeLevel inputs.dt

flatten : Vec3 -> Vec3
flatten v =
    let r = toRecord v
    in  normalize (vec3 r.x 0 r.z)

turn : EyeLevel -> Float -> Float -> Model.Person -> Model.Person
turn eyeLevel dx dy person =
    let
        mx = if getY person.pos > (eyeLevel person.pos) + 5 then 10.0 else 1.0
        h' = person.horizontalAngle + (dx * mx)
        v' = person.verticalAngle   - dy
    in
        { person | horizontalAngle = h'
                 , verticalAngle = clamp (degrees -90) (degrees 90) v'
                 -- , verticalAngle = v'
        }

fly : { a | x:Float, y:Float, dt:Float } -> Model.Person -> Model.Person
fly directions person =
    let moveDir = normalize (Model.direction person)
        strafeDir = transform (makeRotate (degrees -90) j) moveDir

        move = V3.scale (8.0 * directions.y) moveDir
        strafe = V3.scale (8.0 * directions.x) strafeDir
    in
        { person | velocity = adjustVelocity 50 0.01 (move `add` strafe) directions.dt person.velocity }

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
    -- v3_clamp 20 <| add (V3.scale (1.0-(friction*dt)) dv) (V3.scale (1.0-((1.0-friction)*dt)) v)
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
