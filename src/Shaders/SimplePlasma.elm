module Shaders.SimplePlasma (simplePlasma) where

import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import WebGL exposing (..)

-- https://www.shadertoy.com/view/ldBGRR
simplePlasma : Shader {} { u | iResolution:Vec3, iGlobalTime:Float } { elm_FragColor:Vec3, elm_FragCoord:Vec2 }
simplePlasma = [glsl|

precision mediump float;
uniform vec3 iResolution;
uniform float iGlobalTime;

varying vec3 elm_FragColor;
varying vec2 elm_FragCoord;

void main(void)
{
	//vec2 p = -1.0 + 2.0 * gl_FragCoord.xy / iResolution.xy;
	vec2 p = -1.0 + 2.0 * elm_FragCoord.xy;
	
// main code, *original shader by: 'Plasma' by Viktor Korsun (2011)
float x = p.x;
float y = p.y;
float mov0 = x+y+cos(sin(iGlobalTime)*2.0)*100.+sin(x/100.)*1000.;
float mov1 = y / 0.9 +  iGlobalTime;
float mov2 = x / 0.2;
float c1 = abs(sin(mov1+iGlobalTime)/2.+mov2/2.-mov1-mov2+iGlobalTime);
float c2 = abs(sin(c1+sin(mov0/1000.+iGlobalTime)+sin(y/40.+iGlobalTime)+sin((x+y)/100.)*3.));
float c3 = abs(sin(c2+cos(mov1+mov2+c2)+cos(mov2)+sin(x/1000.)));
gl_FragColor = vec4(c1,c2,c3,1);
	
}

|]
