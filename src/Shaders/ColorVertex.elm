module Shaders.ColorVertex (ColorVertex, colorVertex) where

import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import WebGL exposing (..)

type alias ColorVertex = { pos:Vec3, color:Vec3 }

colorVertex : Shader ColorVertex { u | view:Mat4 } { vcolor:Vec3 }
colorVertex = [glsl|

attribute vec3 pos;
attribute vec3 color;
uniform mat4 view;
varying vec3 vcolor;

void main () {
    gl_Position = view * vec4(pos, 1.0);
    vcolor = color;
}

|]
