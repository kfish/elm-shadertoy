module Shaders.ColorFragment (colorFragment) where

import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import WebGL exposing (..)

colorFragment : Shader {} u { vcolor:Vec3 }
colorFragment = [glsl|

precision mediump float;
varying vec3 vcolor;

void main () {
    gl_FragColor = vec4(vcolor, 1.0);
}

|]
