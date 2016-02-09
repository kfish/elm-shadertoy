module Things.Sphere (spheres, cloudsSphere, fogMountainsSphere, sphere) where

import Random exposing (float, list)
import List exposing (drop, concat, map, map2)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import Time exposing (inSeconds)
import WebGL exposing (..)

import Signal.Extra exposing ((<~))

import Shaders.Clouds exposing (clouds)
import Shaders.Fire exposing (fire)
import Shaders.FogMountains exposing (fogMountains)
--import Shaders.SimplePlasma exposing (simplePlasma)
--import Shaders.VoronoiDistances exposing (voronoiDistances)
import Shaders.WorldVertex exposing (Vertex, worldVertex)

import Model
import Engine exposing (..)

type alias Triangle a = (a,a,a)

spheres n fragmentShader = map (always (sphere worldVertex fragmentShader)) [0..n]

-- cloudsSphere : (Int,Int) -> Time -> Mat4 -> Renderable
cloudsSphere : Oriented (Visible {})
cloudsSphere = sphere worldVertex clouds

-- fogMountainsSphere : (Int,Int) -> Time -> Mat4 -> Renderable
-- fogMountainsSphere : Signal (Oriented (Visible a))
fogMountainsSphere = sphere worldVertex fogMountains

sphere vertexShader fragmentShader =
    let see = seeSphere vertexShader fragmentShader
    in { pos = vec3 0 0 0, orientation = vec3 1 0 1, see = see }

type alias ShadertoyUniforms a = { a | iResolution : Vec3, iGlobalTime : Float, view : (Int,Int) }

-- sphere : Shader attributes uniforms varying -> Shader {} uniforms varyings
--    -> (Int,Int) -> Time -> Mat4 -> Renderable
-- sphere : Shader attributes (ShadertoyUniforms {}) varyings -> Shader {} (ShadertoyUniforms {})  varyings -> Perception -> Renderable
seeSphere vertexShader fragmentShader p =
    let (w,h) = p.resolution
        resolution = vec3 (toFloat w) (toFloat h) 0
        s = inSeconds p.globalTime
    in
        [render vertexShader fragmentShader sphereMesh
            { iResolution=resolution, iGlobalTime=s, view=p.viewMatrix }]

unfold : Int -> (a -> a) -> a -> List a
unfold n f x = if n==0 then [] else
  let res=f x in (res :: unfold (n-1) f res)

zip3 : List a -> List b -> List c -> List (a,b,c)
zip3 xs ys zs =
  case (xs, ys, zs) of
    (x::xs', y::ys', z::zs') -> (x,y,z) :: zip3 xs' ys' zs'
    _ -> []

rotY n = makeRotate (2*pi/n) (vec3 0 1 0)
rotZ n = makeRotate (-2*pi/n) (vec3 0 0 1)

rotBoth : Float -> Vertex -> Vertex
rotBoth n x = { pos = transform (rotY n) x.pos, coord = transform (rotZ n) x.coord }

rotMercator : Float -> Vertex -> Vertex
rotMercator n v = { pos = transform (rotY n) v.pos,
    coord = vec3 (getX v.coord + (1.0/n)) (getY v.coord) 0 }

seven : Vertex -> List Vertex
seven = unfold 7 (rotMercator 8)

eights x = let x7 = seven x in (x::x7, x7++[x])

unfoldMercator : Int -> Vertex -> List Vertex
unfoldMercator n = unfold (n-1) (rotMercator (toFloat n))

verticesMercator n x = let xs = unfoldMercator n x in (x::xs, xs++[x])

-- sphereMesh : List (Triangle Vertex)
sphereMesh : Drawable { pos:Vec3, coord:Vec3 }
sphereMesh =
  let
      npole = { pos = vec3 0 1 0, coord = vec3 0 0 0 }
      spole = { pos = vec3 0 -1 0, coord = vec3 0 1 0 }

      nlat q = let x = sqrt (1-q*q) in { pos = vec3 x (-q) 0, coord = vec3 0 ((1-q)/2) 0 }
      slat q = let x = sqrt (1-q*q) in { pos = vec3 x q 0, coord = vec3 0 ((1+q)/2) 0 }

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
      qs = map (sin << (\x -> x*pi/2)) (qs0 30)
      nbands = concat (map2 nband qs (drop 1 qs))
      sbands = concat (map2 sband qs (drop 1 qs))
  in
      Triangle <| nbands ++ sbands
