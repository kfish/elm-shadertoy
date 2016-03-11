module Vehicles.DreamBird (move, welcome) where

import Math.Vector3 exposing (..)
import Math.Vector3 as V3
import Math.Matrix4 exposing (..)
import Math.Quaternion as Qn
import Util exposing (v3_clamp)

import Array2D exposing (Array2D)
import Model
import Things.Terrain as Terrain

import Debug

----------------------------------------------------------------------
-- DreamBird

move : Model.EyeLevel -> Model.Inputs -> Model.Person -> Model.Person
move eyeLevel inputs person = 
    person |> fly eyeLevel inputs
           |> flyPhysics eyeLevel inputs.dt

-- | Welcome a new driver to the DreamBird
welcome : Model.Person -> Model.Person
welcome person = person

-- http://www.dtic.mil/dtic/tr/fulltext/u2/a152616.pdf
fly : Model.EyeLevel -> Model.Inputs -> Model.Person -> Model.Person
fly eyeLevel inputs person =
    let
        thrust = inputs.y

        yaw   = -5 * inputs.x  * inputs.dt
        pitch =  4 * inputs.my * inputs.dt
        roll  =  6 * inputs.mx * inputs.dt

        orientQn = Qn.hamilton person.orientQn (Qn.fromEuler (roll, pitch, yaw))
        orient = Qn.vrotate orientQn
        dv = V3.scale (50 * thrust * inputs.dt) <| orient V3.k
        du = V3.scale (20 * thrust * inputs.dt) <| orient V3.j
        dv' = dv `add` du

        vel = (V3.scale 0.8 dv') `add` (V3.scale 0.95 person.velocity)
        
    in
        { person | orientQn = orientQn
                 , velocity = vel
        }

flyPhysics : Model.EyeLevel -> Float -> Model.Person -> Model.Person
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

