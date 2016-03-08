module Math.Quaternion where

import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector3 as V3
import Math.Vector4 exposing (Vec4, vec4)
import Math.Vector4 as V4

{-| Quaternion type -}
type alias Quaternion = Vec4

{-| Creates a new 4-element quaternion with the given x, y, z, and w values. -}
quaternion : Float -> Float -> Float -> Float -> Quaternion
quaternion = V4.vec4

{-| Construction of a scalar quaternion -}
fromScalar : Float -> Quaternion
fromScalar s = vec4 s 0 0 0

{-| Construction of a right quaternion -}
fromVec3 : Vec3 -> Quaternion
fromVec3 v =
    let {x,y,z} = V3.toRecord v
    in vec4 0 x y z

{-| Extract the scalar component of a quaternion. -}
getScalar : Quaternion -> Float
getScalar = V4.getX

{-| Extract the i component of a quaternion. -}
getI : Quaternion -> Float
getI = V4.getY

{-| Extract the j component of a quaternion. -}
getJ : Quaternion -> Float
getJ = V4.getZ

{-| Extract the k component of a quaternion. -}
getK : Quaternion -> Float
getK = V4.getW

{-| Update the scalar component of a quaternion, returning a new quaternion. -}
setScalar : Float -> Quaternion -> Quaternion
setScalar = V4.setX

{-| Update the i component of a quaternion, returning a new quaternion. -}
setI : Float -> Quaternion -> Quaternion
setI = V4.setY

{-| Update the j component of a quaternion, returning a new quaternion. -}
setJ : Float -> Quaternion -> Quaternion
setJ = V4.setZ

{-| Update the k component of a quaternion, returning a new quaternion. -}
setK : Float -> Quaternion -> Quaternion
setK = V4.setW

{-| Convert a quaternion to a tuple. -}
toTuple : Quaternion -> (Float,Float,Float,Float)
toTuple = V4.toTuple

{-| Convert a tuple to a quaternion. -}
fromTuple : (Float,Float,Float,Float) -> Quaternion
fromTuple = V4.fromTuple

{-| Convert a quaternion to a record. -}
toRecord : Quaternion -> { s:Float, i:Float, j:Float, k:Float }
toRecord q =
    let {x,y,z,w} = V4.toRecord q
    in { s=x, i=y, j=z, k=w }

{-| Convert a record to a quaternion. -}
fromRecord : { s:Float, i:Float, j:Float, k:Float } -> Quaternion
fromRecord {s,i,j,k} = quaternion s i j k

toSV : Quaternion -> (Float, Vec3)
toSV q =
    let {x,y,z,w} = V4.toRecord q
    in (x, vec3 y z w)

fromSV : (Float, Vec3) -> Quaternion
fromSV (s,v) =
    let {x,y,z} = V3.toRecord v
    in quaternion s x y z

{-| Quaternion addition: a + b -}
add : Quaternion -> Quaternion -> Quaternion
add = V4.add

{-| Quaternion subtraction: a - b -}
sub : Quaternion -> Quaternion -> Quaternion
sub = V4.sub

{-| Quaternion negation: -a -}
negate : Quaternion -> Quaternion
negate = V4.negate

{-| The length of the given quaternion: |a| -}
length : Quaternion -> Float
length = V4.length

{-| The square of the length of the given quaternion: |a| * |a| -}
lengthSquared : Quaternion -> Float
lengthSquared = V4.lengthSquared

{-| A unit quaternion with the same direction as the given quaternion: a / |a| -}
normalize : Quaternion -> Quaternion
normalize = V4.normalize

{-| Multiply the quaternion by a scalar: s * q -}
scale : Float -> Quaternion -> Quaternion
scale = V4.scale

{-| Extract the axis of rotation -}
toVec3 : Quaternion -> Vec3
toVec3 q = let {x,y,z,w} = V4.toRecord q in vec3 y z w

{-| Quaternion conjugate -}
conjugate : Quaternion -> Quaternion
conjugate q =
    let {x,y,z,w} = V4.toRecord q
    in fromRecord { s = x , i = -y, j = -z, k = -w }

{-| Hamilton product -}
hamilton : Quaternion -> Quaternion -> Quaternion
hamilton q1 q2 =
    let (s1, v1) = toSV q1
        (s2, v2) = toSV q2
        s = s1*s2 - V3.dot v1 v2
        v3_add = V3.add
        v = V3.scale s1 v2 `v3_add` V3.scale s2 v1 `v3_add` V3.cross v1 v2
    in fromSV (s,v)

{-| Multiplication of a quaternion by a vector -}
multv : Quaternion -> Vec3 -> Quaternion
multv q v = hamilton q (fromVec3 v)

{-| Multiplication of a vector by a quaternion -}
vmult : Vec3 -> Quaternion -> Quaternion
vmult v q = hamilton (fromVec3 v) q

{-| Angle of rotation -}
getAngle : Quaternion -> Float
getAngle q = 2.0 * acos (getScalar q)

{-| Unit vector along the axis of rotation -}
getAxis : Quaternion -> Vec3
getAxis q = V3.normalize (toVec3 q)

{-| Rotate quaternion q1 by quaternion q2 -}
rotate : Quaternion -> Quaternion -> Quaternion
rotate q1 q2 = hamilton q1 (hamilton q2 (conjugate q1))

{-| Rotate a vector v by the unit quaternion q -}
vrotate : Quaternion -> Vec3 -> Vec3
-- vrotate q v = toVec3 <| hamilton (multv q v) (conjugate q)
vrotate q v = toVec3 <| hamilton q (vmult v (conjugate q))

{-| Construction from Euler angles representing roll, pitch, yaw.
often denoted phi, tau, psi -}
fromEuler : Float -> Float -> Float -> Quaternion
fromEuler phi tau psi =
    let
        roll  = quaternion (cos (phi/2)) 0 0 (sin (phi/2))
        pitch = quaternion (cos (tau/2)) (sin (tau/2)) 0 0
        yaw   = quaternion (cos (psi/2)) 0 (sin (psi/2)) 0
    in yaw `hamilton` pitch `hamilton` roll
{-
    let
        sphi = sin (phi/2)
        cphi = cos (phi/2)
        stau = sin (tau/2)
        ctau = cos (tau/2)
        spsi = sin (psi/2)
        cpsi = cos (psi/2)

        s = cphi * ctau * cpsi + sphi * stau * spsi
        i = sphi * ctau * cpsi - cphi * stau * spsi
        j = cphi * stau * cpsi + sphi * ctau * spsi
        k = cphi * ctau * spsi - sphi * stau * cpsi
    in quaternion s j k i
-}
        
{-| Convert to Euler angles representing (roll, pitch, yaw),
often denoted (phi, tau, psi) -}
toEuler : Quaternion -> (Float, Float, Float)
toEuler q =
    let
        {s,i,j,k} = toRecord q
        q00 = s * s
        q11 = i * i
        q22 = j * j
        q33 = k * k
        r11 = q00 + q11 - q22 - q33
        r21 = 2 * (i*j + s*k)
        r31 = 2 * (i*k - s*j)
        r32 = 2 * (j*k + s*i)
        r33 = q00 - q11 - q22 + q33

        tmp = abs r31
    in
        if (tmp > 0.999999) then
            let    
                r12 = 2 * (i*j - s*k)
                r13 = 2 * (i*k + s*j)
                roll = 0
                pitch = -(pi/2) * r31/tmp
                yaw = atan2 -r12 (-r31*r13)
            in (roll, pitch, yaw)
        else
            let    
                roll = atan2 r32 r33
                pitch = asin -r31
                yaw = atan2 r21 r11
            in (roll, pitch, yaw)
