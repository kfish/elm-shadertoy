# Shadertoy for Elm

[Shadertoy](http://shadertoy.com/) is a community of shader developers, sharing code
with online demos.

[Elm](http://elm-lang.org/) is a very concise language with Web GL support.

The purpose of this project is to make it easy to incorporate shaders from Shadertoy
in Elm projects.

The significance is that the work of these two outstanding developer communities can
be multiplied: Elm developers can learn advanced shader techniques, and shader
developers can easily build larger interactive systems.

## How it works

You can define vertex meshes, game logic and physics very clearly in Elm, then
write your shaders directly in GLSL. The Elm compiler translates the Elm code to
Javascript and checks the GLSL for parse errors. On the client web browser, the
GLSL is interpreted by the system OpenGL system, such as the GPU driver.

Make sure you have the latest version of Chrome or Firefox and then click the
following image to try out the **[live demo][demo]**:

[![Live Demo](resources/ScreenShot.png)][demo]

[demo]: http://kfish.github.io/elm-shadertoy/

## Importing Shadertoy fragment shaders

Shaders in Elm are included verbatim in a `[glsl| ... ]` block.

Use the following types and preamble to define a fragment shader named `foo`:

```glsl
foo : Shader {} { u | iResolution:Vec3, iGlobalTime:Float } { elm_FragCoord:Vec2 }
foo = [glsl|

precision mediump float;
uniform vec3 iResolution;
uniform float iGlobalTime;

varying vec2 elm_FragCoord;

<<<SHADER CODE GOES HERE>>>

|]
```

### Inputs

Shadertoy defines various inputs to fragment shaders. elm-shadertoy provides
compatibility for the following:


```glsl
uniform vec3      iResolution;           // viewport resolution (in pixels)
uniform float     iGlobalTime;           // shader playback time in seconds
```

elm-shadertoy additionally defines 'elm_FragCoord'.

```glsl
varying vec2      elm_FragCoord;         // texture-space fragment coordinate
```

Replace all occurrences of `gl_FragCoord.xy / iResolution.xy` with `elm_FragCoord.xy`. This
ensures that pixels are calculated according to their location on 3D surface, rather than
their location on your 2D screen.


### What is NOT (YET) supported

The following Shadertoy inputs are not yet supported by elm-shadertoy:

```
uniform float     iChannelTime[4];       // channel playback time (in seconds)
uniform vec3      iChannelResolution[4]; // channel resolution (in pixels)
uniform vec4      iMouse;                // mouse pixel coords. xy: current (if MLB down), zw: click
uniform samplerXX iChannel0..3;          // input channel. XX=2D/Cube
uniform vec4      iDate;                 // (year, month, day, time in seconds)
```

These are tracked in issues in this project (elm-shadertoy):
  1. [Support Shadertoy channel inputs](https://github.com/kfish/elm-shadertoy/issues/1)
  2. [Support Shadertoy mouse input](https://github.com/kfish/elm-shadertoy/issues/2)
  3. [Support Shadertoy date input](https://github.com/kfish/elm-shadertoy/issues/3)

Additionally, there is an issue in the Haskell language-glsl package regarding the
[GLSL preprocessor: support for #define, #ifdef etc.](https://github.com/noteed/language-glsl/issues/4),
so you need to manually preprocess (replace constants by variables,
use comments to select behavior instead of #ifdef).

## Build Locally

After installing [the Elm Platform](https://github.com/elm-lang/elm-platform),
run the following sequence of commands:

```bash
git clone https://github.com/kfish/elm-shadertoy.git
cd elm-shadertoy
elm-get install
elm-make src/Main.elm --output build/Main.js
elm-reactor
```

And then open [http://localhost:8000](http://localhost:8000) to see it in action!

## Credits

The Elm code here is forked from 
Evan Czaplicki's [first-person-elm](https://github.com/evancz/first-person-elm) demo.

Although the broader purpose of this project is to make it easy to use any shader from Shadertoy,
this demo in particular includes the following shaders:

  * [Fire](https://www.shadertoy.com/view/Xsl3zN) by 301
  * [Fog Mountains](https://www.shadertoy.com/view/XdsGD7) by ESpitz
  * [Simple Plasma](https://www.shadertoy.com/view/ldBGRR) by Kastor
  * [Voronoi distances](https://www.shadertoy.com/view/ldl3W8) by iq

