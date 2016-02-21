module Update (step) where

import Math.Vector3 exposing (..)
import Math.Vector3 as V3
import Math.Matrix4 exposing (..)
import Model

step : Model.Inputs -> Model.Person -> Model.Person
step inputs person =
    case inputs of
      Model.Mouse movement -> turn movement person
      Model.TimeDelta isJumping directions dt ->
          person |> fly directions
                 |> physics dt
{-
          person |> walk directions
                 |> jump isJumping
                 |> gravity dt
                 |> physics dt
-}

flatten : Vec3 -> Vec3
flatten v =
    let r = toRecord v
    in  normalize (vec3 r.x 0 r.z)

turn : (Int,Int) -> Model.Person -> Model.Person
turn (dx,dy) person =
    let yo x = toFloat (clamp -10 10 x) / 500
        h' = person.horizontalAngle + yo dx
        v' = person.verticalAngle   - yo dy
    in
        { person | horizontalAngle = h'
                 , verticalAngle = clamp (degrees -45) (degrees 45) v'
        }

fly : { x:Int, y:Int } -> Model.Person -> Model.Person
fly directions person =
  -- if getY person.pos > Model.eyeLevel then person else
    let moveDir = normalize (Model.direction person)
        strafeDir = transform (makeRotate (degrees -90) j) moveDir

        move = V3.scale (8.0 * toFloat directions.y) moveDir
        strafe = V3.scale (8.0 * toFloat directions.x) strafeDir
    in
        { person | velocity = adjustVelocity (move `add` strafe) }

walk : { x:Int, y:Int } -> Model.Person -> Model.Person
walk directions person =
  if getY person.pos > Model.eyeLevel then person else
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
      _       -> V3.scale 8 (normalize v)

jump : Bool -> Model.Person -> Model.Person
jump isJumping person =
  if not isJumping || getY person.pos > Model.eyeLevel then person else
    let v = toRecord person.velocity
    in
        { person | velocity = vec3 v.x 2 v.z }

physics : Float -> Model.Person -> Model.Person
physics dt person =
    let pos = person.pos `add` V3.scale dt person.velocity
        p = toRecord pos

        pos' = if p.y < Model.eyeLevel
                    then vec3 p.x Model.eyeLevel p.z
                    else pos
    in
        { person | pos = pos' }

gravity : Float -> Model.Person -> Model.Person
gravity dt person =
  if getY person.pos <= Model.eyeLevel then person else
    let v = toRecord person.velocity
    in
        { person | velocity = vec3 v.x (v.y - 2 * dt) v.z }
