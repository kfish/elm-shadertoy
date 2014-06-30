module Things.Sphere (cloudsSphere, fogMountainsSphere, sphere) where

import Math.Vector2 (Vec2)
import Math.Vector3 (..)
import Math.Matrix4 (..)
import Graphics.WebGL (..)

import Shaders.Clouds (clouds)
import Shaders.Fire (fire)
import Shaders.FogMountains (fogMountains)
--import Shaders.SimplePlasma (simplePlasma)
--import Shaders.VoronoiDistances (voronoiDistances)
import Shaders.WorldVertex (Vertex, worldVertex)

import Model
import Engine (..)

-- cloudsSphere : (Int,Int) -> Time -> Mat4 -> Entity
cloudsSphere : Signal Thing
cloudsSphere = constant <| sphere worldVertex clouds

-- fogMountainsSphere : (Int,Int) -> Time -> Mat4 -> Entity
fogMountainsSphere : Signal Thing
fogMountainsSphere = constant <| sphere worldVertex fogMountains

-- type ShadertoyUniforms a = { a | iResolution : Vec3, iGlobalTime : Float, view : (Int,Int) }

-- sphere : Shader attributes uniforms varying -> Shader {} uniforms varyings
--    -> (Int,Int) -> Time -> Mat4 -> Entity
-- sphere : Shader attributes (ShadertoyUniforms {}) varyings -> Shader {} (ShadertoyUniforms {})  varyings -> Perception -> Entity
sphere vertexShader fragmentShader p =
    let (w,h) = p.resolution
        resolution = vec3 (toFloat w) (toFloat h) 0
        s = inSeconds p.globalTime
    in
        entity vertexShader fragmentShader sphereMesh
            { iResolution=resolution, iGlobalTime=s, view=p.viewMatrix }

unfold : Int -> (a -> a) -> a -> [a]
unfold n f x = if n==0 then [] else
  let res=f x in (res :: unfold (n-1) f res)

zip3 : [a] -> [b] -> [c] -> [(a,b,c)]
zip3 xs ys zs =
  case (xs, ys, zs) of
    (x::xs', y::ys', z::zs') -> (x,y,z) :: zip3 xs' ys' zs'
    _ -> []

rotY n = makeRotate (2*pi/n) (vec3 0 1 0)
rotZ n = makeRotate (-2*pi/n) (vec3 0 0 1)

rotBoth : Float -> Vertex -> Vertex
rotBoth n x = { position = transform (rotY n) x.position, coord = transform (rotZ n) x.coord }

rotMercator : Float -> Vertex -> Vertex
rotMercator n v = { position = transform (rotY n) v.position,
    coord = vec3 (getX v.coord + (1.0/n)) (getY v.coord) 0 }

seven : Vertex -> [Vertex]
seven = unfold 7 (rotMercator 8)

eights x = let x7 = seven x in (x::x7, x7++[x])

unfoldMercator : Int -> Vertex -> [Vertex]
unfoldMercator n = unfold (n-1) (rotMercator (toFloat n))

verticesMercator n x = let xs = unfoldMercator n x in (x::xs, xs++[x])

sphereMesh : [Triangle Vertex]
sphereMesh =
  let
      npole = { position = vec3 0 1 0, coord = vec3 0 0 0 }
      spole = { position = vec3 0 -1 0, coord = vec3 0 1 0 }

      nlat q = let x = sqrt (1-q*q) in { position = vec3 x (-q) 0, coord = vec3 0 ((1-q)/2) 0 }
      slat q = let x = sqrt (1-q*q) in { position = vec3 x q 0, coord = vec3 0 ((1+q)/2) 0 }

      nband q1 q2 = 
          let
              (band10, band11) = verticesMercator 20 (nlat q1)
              (band20, band21) = verticesMercator 20 (nlat q2)
              band1U = zip3 band10 band11 band20
              band1L = zip3 band20 band11 band21
          in band1U ++ band1L

      sband q1 q2 = 
          let
              (band10, band11) = verticesMercator 20 (slat q1)
              (band20, band21) = verticesMercator 20 (slat q2)
              band1U = zip3 band10 band11 band20
              band1L = zip3 band20 band11 band21
          in band1U ++ band1L

      qs0 n = map (\x -> x/n) [0..n]
      qs = map (sin . (\x -> x*pi/2)) (qs0 30)
      nbands = concat (zipWith nband qs (drop 1 qs))
      sbands = concat (zipWith sband qs (drop 1 qs))
  in
      nbands ++ sbands
