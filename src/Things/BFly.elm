module Things.BFly (fireBFly, voronoiBFly) where

import Random (float, floatList)
import Math.Vector2 (Vec2)
import Math.Vector3 (..)
import Math.Matrix4 (..)
import Graphics.WebGL (..)

import Shaders.Fire (fire)
import Shaders.VoronoiDistances (voronoiDistances)

import Model
import Engine (..)

import Debug (log)

type Vertex = { position:Vec3, coord:Vec3, wing:Vec3 }

fireBFly : Signal Thing
fireBFly = bfly bflyVertex fire <~ ((\x -> x * second * pi*2) <~ float (constant 7))

voronoiBFlys : Int -> Signal [Thing]
voronoiBFlys n = map (bfly bflyVertex voronoiDistances . (\x -> x * second * pi * 2)) <~ floatList (constant n)

voronoiBFly : Signal Thing
voronoiBFly = bfly bflyVertex voronoiDistances <~ ((\x -> x * second * pi*2) <~ float (constant 7))

bfly vertexShader fragmentShader flapStart p =
    let (w,h) = p.resolution
        resolution = vec3 (toFloat w) (toFloat h) 0
        -- s = log (show flapStart) <| inSeconds (p.globalTime + flapStart)
        s = inSeconds (p.globalTime + flapStart)
        flap = 0.3 + (sin (s*8) + 1)/2
        flapL = makeRotate (-flap * pi/4) (vec3 0 0 1)
        flapR = makeRotate (flap * pi/4) (vec3 0 0 1)
    in
        entity vertexShader fragmentShader mesh
            { iResolution=resolution, iGlobalTime=s, view=p.viewMatrix, flapL=flapL, flapR=flapR }

mesh : [Triangle Vertex]
mesh =
    let bHead  = Vertex (vec3 0 0 0.5) (vec3 0.5 0 0) (vec3 0 0 0)
        bTail  = Vertex (vec3 0 0 -0.5) (vec3 0.5 1 0) (vec3 0 0 0)
        bLeft  = Vertex (vec3 -1.7 0 -0.1) (vec3 0 0.5 0) (vec3 -1 0 0)
        bRight = Vertex (vec3 1.7 0 -0.1)  (vec3 1 0.5 0) (vec3 1 0 0)
    in
        [ (bHead, bTail, bLeft), (bHead, bTail, bRight) ]

bflyVertex : Shader Vertex { u | view:Mat4, flapL:Mat4, flapR:Mat4 } { elm_FragCoord:Vec2 }
bflyVertex = [glsl|

attribute vec3 position;
attribute vec3 coord;
attribute vec3 wing;
uniform mat4 view;
uniform mat4 flapL;
uniform mat4 flapR;
varying vec2 elm_FragCoord;
void main () {
  mat4 flap;
  if (wing.x < 0.0) { flap = flapL; }
  else if (wing.x > 0.0) { flap = flapR; }
  else { flap = mat4(1.0); }
  gl_Position = view * flap * vec4(position, 1.0);
  elm_FragCoord = coord.xy;
}

|]
