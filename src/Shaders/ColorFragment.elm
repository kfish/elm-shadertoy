module Shaders.ColorFragment (colorFragment, noiseColorFragment) where

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

-- TODO: Configure
--  * size of fractal detail (multiplier on elm_FragCoord)
--  * speed of motion (multiplier on iGlobalTime): can be motionless for solid objects
--  * swaying in the breeze (oscillate with sin)

-- TODO: make surface2D tile seamlessly

noiseColorFragment : Shader {} { u | iResolution:Vec3, iGlobalTime:Float } { elm_FragColor:Vec3, elm_FragCoord:Vec2, iTextureScale:Float, iTimeScale:Float }
noiseColorFragment = [glsl|

precision mediump float;
uniform vec3 iResolution;
uniform float iGlobalTime;

varying vec3 elm_FragColor;
varying vec2 elm_FragCoord;
varying float iTextureScale;
varying float iTimeScale;

// by @301z

float rand(vec2 n) { 
	return fract(sin(dot(n, vec2(12.9898, 4.1414))) * 43758.5453);
}

float noise(vec2 n) {
	const vec2 d = vec2(0.0, 1.0);
	vec2 b = floor(n), f = smoothstep(vec2(0.0), vec2(1.0), fract(n));
	return mix(mix(rand(b), rand(b + d.yx), f.x), mix(rand(b + d.xy), rand(b + d.yy), f.x), f.y);
}

float fbm(vec2 n) {
	float total = 0.0, amplitude = 1.0;
	for (int i = 0; i < 7; i++) {
		total += noise(n) * amplitude;
		n += n;
		amplitude *= 0.5;
	}
	return total;
}

void main() {
	vec3 c1 = elm_FragColor;
	vec3 c2 = elm_FragColor * vec3(0.7, 0.7, 0.7);
	vec3 c3 = elm_FragColor * vec3(0.6, 0.6, 0.6);
	vec3 c4 = elm_FragColor * vec3(0.9, 0.9, 0.9);
	vec3 c5 = vec3(0.10);
	vec3 c6 = vec3(0.50);

	vec2 p = elm_FragCoord.xy * iTextureScale;

        float scaledTime = iGlobalTime * iTimeScale;
	float q = fbm(p - scaledTime * 0.1);
	vec2 r = vec2(fbm(p + q + scaledTime * 0.7 - p.x - p.y), fbm(p + q - scaledTime * 0.4));
	vec3 c = mix(c1, c2, fbm(p + r)) + mix(c3, c4, r.x) - mix(c5, c6, r.y);
	gl_FragColor = vec4(c * cos(1.57 * gl_FragCoord.y / iResolution.y), 1.0);
}

|]
