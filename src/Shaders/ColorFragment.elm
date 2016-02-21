module Shaders.ColorFragment (colorFragment) where

import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import WebGL exposing (..)

colorFragment : Shader {} u { elm_FragColor:Vec3, elm_FragCoord:Vec2 }
colorFragment = [glsl|

precision mediump float;

varying vec3 elm_FragColor;
varying vec2 elm_FragCoord;

void main () {
    gl_FragColor = vec4(elm_FragColor, 1.0);
}

|]
