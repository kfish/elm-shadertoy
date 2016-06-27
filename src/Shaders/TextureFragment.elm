module Shaders.TextureFragment exposing (textureFragment)

import Math.Vector2 exposing (Vec2)
import WebGL exposing (Shader, Texture)

textureFragment : Shader {} { u | iTexture:Texture } { elm_FragCoord:Vec2 }
textureFragment = [glsl|

precision mediump float;
uniform sampler2D iTexture;

varying vec2 elm_FragCoord;

void main() {
    gl_FragColor = texture2D(iTexture, elm_FragCoord);
}

|]
