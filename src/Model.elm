module Model where

import Math.Vector3 exposing (Vec3, vec3)

type Inputs
    = TimeDelta Bool {x:Int, y:Int} Float
    | Mouse (Int,Int)

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
    , horizontalAngle : Float
    , verticalAngle   : Float
    }

eyeLevel : Float
eyeLevel = 10

defaultPerson : Person
defaultPerson =
    { pos = vec3 0 eyeLevel -8
    , velocity = vec3 0 0 0
    , horizontalAngle = degrees 90
    , verticalAngle = 0
    }

direction : Person -> Vec3
direction person =
    let h = person.horizontalAngle
        v = person.verticalAngle
    in
        vec3 (cos h) (sin v) (sin h)
