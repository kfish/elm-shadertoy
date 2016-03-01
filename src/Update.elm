module Update (step) where

import Math.Vector3 exposing (..)
import Math.Vector3 as V3
import Math.Matrix4 exposing (..)

import Array2D exposing (Array2D)
import Model

type alias EyeLevel = Vec3 -> Float

step : Array2D Float -> Model.Inputs -> Model.Person -> Model.Person
step terrain inputs person =
    case inputs of
      Model.Mouse movement -> turn movement person
      Model.TimeDelta isJumping directions dt ->
        let 
            eyeLevel pos = Model.eyeLevel + elevation terrain pos
        in
{-
          person |> fly directions
                 |> physics eyeLevel dt
-}
          person |> walk eyeLevel directions
                 |> jump eyeLevel isJumping
                 |> gravity eyeLevel dt
                 |> physics eyeLevel dt

{-
elevation : Array2D Float -> Vec3 -> Float
elevation terrain pos =
    let
        ix0 = floor <| (getX pos + 256) / 2
        iz0 = floor <| (getZ pos + 256) / 2
        getXZ x z = (Array2D.getXY x z 0 terrain) * 80
    in
        getXZ ix0 iz0
-}

-- Elevation of terrain at a given coordinate
-- Linearly interpolated on the mesh triangle
-- TODO: move this to the Terrain module
elevation : Array2D Float -> Vec3 -> Float
elevation terrain pos =
    let
        ix0 = (getX pos + 256) / 2
        ix  = floor ix0
        ixf = ix0 - toFloat ix

        iz0 = (getZ pos + 256) / 2
        iz  = floor iz0
        izf = iz0 - toFloat iz

        getXZ x z = (Array2D.getXY x z 0 terrain) * 80

        i00 = getXZ ix     iz      --     00 ... 10  -> x
        i10 = getXZ (ix+1) iz      --  |  .    /  .
                                   --  v  .   /   .
        i01 = getXZ ix     (iz+1)  --     .  /    .
        i11 = getXZ (ix+1) (iz+1)  --  z  01 ... 11

        mix a b f = (a * (1-f) + b * f) / 2 -- f describes how close to a

    in
        if ixf + izf < 1.0 then
            mix i00 i10 ixf + mix i00 i01 izf
        else
            mix i01 i11 ixf + mix i10 i11 izf
            
bounds : Vec3 -> Vec3
bounds pos =
    let bound x low high = if (x < low) then low else (if x > high then high else x)
        (x,y,z) = V3.toTuple pos
    in vec3 (bound x -246 1782) (bound y 0 1000) (bound z -246 1782)

flatten : Vec3 -> Vec3
flatten v =
    let r = toRecord v
    in  normalize (vec3 r.x 0 r.z)

turn : (Int,Int) -> Model.Person -> Model.Person
turn (dx,dy) person =
    -- let yo x = toFloat (clamp -30 30 x) / 500
    let yo x = toFloat x / 500
        h' = person.horizontalAngle + yo dx
        v' = person.verticalAngle   - yo dy
    in
        { person | horizontalAngle = h'
                 -- , verticalAngle = clamp (degrees -90) (degrees 90) v'
                 , verticalAngle = v'
        }

fly : { x:Int, y:Int } -> Model.Person -> Model.Person
fly directions person =
    let moveDir = normalize (Model.direction person)
        strafeDir = transform (makeRotate (degrees -90) j) moveDir

        move = V3.scale (8.0 * toFloat directions.y) moveDir
        strafe = V3.scale (8.0 * toFloat directions.x) strafeDir
    in
        { person | velocity = adjustVelocity (move `add` strafe) }

walk : EyeLevel -> { x:Int, y:Int } -> Model.Person -> Model.Person
walk eyeLevel directions person =
  -- if getY person.pos > eyeLevel person.pos then person else
    let moveDir = normalize (flatten (Model.direction person))
        strafeDir = transform (makeRotate (degrees -90) j) moveDir

        move = V3.scale (8.0 * toFloat directions.y) moveDir
        strafe = V3.scale (8.0 * toFloat directions.x) strafeDir
    in
        { person | velocity = adjustVelocity (move `add` strafe) }

adjustVelocity : Vec3 -> Vec3
adjustVelocity v =
    case toTuple v of
      (0,0,0) -> v
      _       -> V3.scale 20 (normalize v)

jump : EyeLevel -> Bool -> Model.Person -> Model.Person
jump eyeLevel isJumping person =
  if not isJumping || getY person.pos > eyeLevel person.pos then person else
    let v = toRecord person.velocity
    in
        { person | velocity = vec3 v.x 2 v.z }

physics : EyeLevel -> Float -> Model.Person -> Model.Person
physics eyeLevel dt person =
    let pos = bounds <| person.pos `add` V3.scale dt person.velocity
        p = toRecord pos
        e = eyeLevel pos

        pos' = if p.y < e
                    then vec3 p.x e p.z
                    else pos
    in
        { person | pos = pos' }

gravity : EyeLevel -> Float -> Model.Person -> Model.Person
gravity eyeLevel dt person =
  if getY person.pos <= eyeLevel person.pos then person else
    let v = toRecord person.velocity
    in
        { person | velocity = vec3 v.x (v.y - (2*80) * dt) v.z }
