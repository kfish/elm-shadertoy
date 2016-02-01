module Shaders.WorldVertex (Vertex, worldVertex) where

import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import WebGL exposing (..)

type alias Vertex = { position:Vec3, coord:Vec3 }

worldVertex : Shader Vertex { u | view:Mat4 } { elm_FragCoord:Vec2 }
worldVertex = [glsl|

attribute vec3 position;
attribute vec3 coord;
uniform mat4 view;
varying vec2 elm_FragCoord;
void main () {
  gl_Position = view * vec4(position, 1.0);
  elm_FragCoord = coord.xy;
}

|]
