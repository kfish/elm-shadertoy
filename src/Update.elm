module Update (step) where

import Math.Vector3 exposing (..)
import Math.Vector3 as V3
import Math.Matrix4 exposing (..)
import Math.Quaternion as Qn
import Util exposing (v3_clamp)

import Array2D exposing (Array2D)
import Model
import Things.Terrain as Terrain
import Vehicles.DreamBird as DreamBird
import Vehicles.DreamBuggy as DreamBuggy

import Debug

step : Array2D Float -> Model.Inputs -> Model.Person -> Model.Person
step terrain inputs person0 = if inputs.reset then Model.defaultPerson else
        let 
            eyeLevel pos = Model.eyeLevel + Terrain.elevation terrain pos
            person = person0
                         |> gravity eyeLevel inputs.dt
                         |> selectVehicle inputs
        in
            if person.flying then
                  person |> DreamBird.move eyeLevel inputs
            else
                  person |> DreamBuggy.move eyeLevel inputs

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
            DreamBird.welcome { person | flying = True }
        else
          Debug.log "Switch to buggy!" <|
            DreamBuggy.welcome { person | flying = False }

gravity : Model.EyeLevel -> Float -> Model.Person -> Model.Person
gravity eyeLevel dt person =
  if getY person.pos <= eyeLevel person.pos then person else
    let v = toRecord person.velocity
    in
        { person | velocity = vec3 v.x (v.y - 9.8 * dt) v.z }
