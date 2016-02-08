module Math.Geometry where

import Math.Vector3 as V3
import Math.Vector3 (Vec3, vec3, dot, cross, add, sub)
import Math.Matrix4 (..)

type Triangle a = (a, a, a)

type Positioned a = { a | pos : Vec3 }

-- Does the ray starting at the given position intersect the triangle?
raySegmentTriangleIntersects : Positioned a -> Vec3 -> Triangle (Positioned b) -> Bool
raySegmentTriangleIntersects src ray tri = case rayTriangleIntersection src ray tri of
    Nothing -> False
    Just t -> t >= 0.0 && t <= 1.0

{- Where does the ray starting at the given position intersect the triangle?
http://www.lighthouse3d.com/tutorials/maths/ray-triangle-intersection/
-}
rayTriangleIntersection : Positioned a -> Vec3 -> Triangle (Positioned b) -> Maybe Float
rayTriangleIntersection src ray (t0, t1, t2) =
    let p = src.pos
        v0 = t0.pos
        v1 = t1.pos
        v2 = t2.pos

        e1 = v1 `cross` v0
        e2 = v2 `cross` v0

        h = ray `cross` e2
        a = e1 `dot` h

    in case (a > -0.00001 && a < 0.00001) of
       True -> Nothing
       False -> let
           f = 1/a
           s = p `sub` v0
           u = f * (s `dot` h)

       in case (u < 0.0 || u > 1.0) of
          True -> Nothing
          False -> let
              q = s `dot` e1
              v = f * (d `dot` q)

          in case (v < 0.0 || u + v > 1.0) of
             True -> Nothing
             False -> let
                 t = f * (e2 `dot` q)
             in case (t > 0.00001) of
                 True -> Just t
                 False -> Nothing
