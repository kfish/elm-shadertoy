module Model where

import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector3 as V3 -- JUST FOR j
import Math.Matrix4 exposing (makeRotate, transform)
import Math.Quaternion as Qn

type alias Inputs =
    { isJumping: Bool
    , x: Float
    , y: Float
    , dt: Float
    , mx: Float
    , my: Float
    , mt: Float
    }

noInput : Inputs
noInput = { isJumping = False, x=0, y=0, dt=0, mx=0, my=0, mt=0 }

-- TODO: Make a new model type with both Person and Things
-- but the Things are passed in and can be added/subtracted
-- ... and maybe set portals separately?
-- ... how to handle collisions between person and things?

{-
type World =
    { person : Person
    , things : [Thing]
    }
-}

type alias Person =
    { pos : Vec3
    , velocity : Vec3
    , pitch : Float
    , roll : Float
    , yaw : Float
    }

eyeLevel : Float
eyeLevel = 1.8

defaultPerson : Person
defaultPerson =
    { pos = vec3 0 eyeLevel 0
    , velocity = vec3 0 0 0
    , yaw = degrees 0
    , pitch = degrees 0
    , roll = degrees 0
    }

orient : Person -> Vec3 -> Vec3
-- orient person = Qn.vrotate (Qn.fromEuler person.roll person.pitch person.yaw)
-- orient person = Qn.vrotate (Qn.fromEuler 0 0 person.roll)
orient person = Qn.vrotate (Qn.fromEuler person.pitch person.yaw person.roll)

direction : Person -> Vec3
direction person = orient person V3.k

cameraUp : Person -> Vec3
cameraUp person = orient person V3.j
