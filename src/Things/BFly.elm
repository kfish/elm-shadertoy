module Things.BFly (bfly) where

import Random exposing (float)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import Signal.Extra exposing ((<~))
import Time exposing (inSeconds, second)
import WebGL exposing (..)

import Model
import Engine exposing (..)

import Debug exposing (log)

type alias BoidVertex = { pos:Vec3, color:Vec3, coord:Vec3, wing:Vec3 }

bfly fragmentShader f01 = makeBFly bflyVertex fragmentShader (f01 * second * pi * 2)

makeBFly vertexShader fragmentShader flapStart =
    let see = seeBFly vertexShader fragmentShader flapStart
    in { pos = (vec3 7 0 4), orientation = vec3 0 0 1, see = see }

seeBFly vertexShader fragmentShader flapStart p =
    let (w,h) = p.resolution
        resolution = vec3 (toFloat w) (toFloat h) 0
        -- s = log (show flapStart) <| inSeconds (p.globalTime + flapStart)
        s = inSeconds (p.globalTime + flapStart)
        flap = -0.1 + (sin (s*8) + 1)/2
        flapL = makeRotate (-flap * 3*pi/8) (vec3 0 0 1)
        flapR = makeRotate (flap * 3*pi/8) (vec3 0 0 1)
    in
        [render vertexShader fragmentShader mesh
            { iResolution=resolution, iGlobalTime=s, view=p.viewMatrix,
              flapL=flapL, flapR=flapR }
        ]

mesh : Drawable BoidVertex
mesh =
    let white  = vec3 1 1 1
        bHead  = { pos = vec3 0 0 0.5, color = white, coord = vec3 0.5 0 0, wing = vec3 0 0 0 }
        bTail  = { pos = vec3 0 0 -0.5, color = white, coord = vec3 0.5 1 0, wing = vec3 0 0 0 }
        bLeft  = { pos = vec3 -0.7 0 -0.7, color = white, coord = vec3 0 0.5 0, wing = vec3 -1 0 0 }
        bRight = { pos = vec3 0.7 0 -0.7, color = white, coord = vec3 1 0.5 0, wing = vec3 1 0 0 }
    in
        Triangle <| [ (bHead, bTail, bLeft), (bHead, bTail, bRight) ]

bflyVertex : Shader BoidVertex { u | view:Mat4, flapL:Mat4, flapR:Mat4 } { elm_FragColor:Vec3, elm_FragCoord:Vec2 }
bflyVertex = [glsl|

attribute vec3 pos;
attribute vec3 color;
attribute vec3 coord;
attribute vec3 wing;
uniform mat4 view;
uniform mat4 flapL;
uniform mat4 flapR;
varying vec3 elm_FragColor;
varying vec2 elm_FragCoord;
void main () {
  mat4 flap;
  if (wing.x < 0.0) { flap = flapL; }
  else if (wing.x > 0.0) { flap = flapR; }
  else { flap = mat4(1.0); }
  gl_Position = view * flap * vec4(pos, 1.0);
  elm_FragColor = color;
  elm_FragCoord = coord.xy;
}

|]
