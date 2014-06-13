module Main where

import String
import Array
import Http


import Math.Vector3 (..)
import Math.Matrix4 (..)
import Graphics.WebGL (..)

type VertV = {position: Vec3}
type VertVT = {position : Vec3, texCoord : Vec3}
type VertVN = {position : Vec3, normal : Vec3}
type VertVTN = {position : Vec3, texCoord : Vec3, normal : Vec3}

data FaceVert =   FaceVertV VertV
  | FaceVertVT VertVT
  | FaceVertVN VertVN
  | FaceVertVTN VertVTN

toVTN f = case f of
  FaceVertVTN vtn -> vtn
  FaceVertVN vn -> {position = vn.position, normal = vn.normal, texCoord = vec3 0 0 0}

data Face = FaceV (VertV, VertV, VertV) 
  | FaceVT (VertVT, VertVT, VertVT)
  | FaceVN (VertVN, VertVN, VertVN)
  | FaceVTN (VertVTN, VertVTN, VertVTN)

--Used to avoid "Math.pow is not a function"
myPow : Float -> Int -> Float
myPow b e = if
  | e < 0 -> 1.0 / (myPow b (0-e))
  | e == 0 -> 1
  | e > 0 -> b * (myPow b (e-1))

--Check if a float is in scientific notation, then parse it accordingly
parseFloat : String -> Float
parseFloat s = case String.split "e" s of
  [fs] -> fromJust <| String.toFloat fs
  [base, power] -> let
      bf = fromJust <| String.toFloat base
      pi = fromJust <| String.toInt power
    in bf  * (myPow 10.0 pi)


--Check the first character of a line for parsing
isVertexLine= (\s -> "v" == (head <| String.words s))
isVtLine= (\s -> String.startsWith "vt" s)
isVnLine= (\s -> String.startsWith "vn" s)
isFaceLine = (\s -> String.startsWith "f" s)

lineToVert : String -> Vec3
lineToVert line = case (String.words line) of
  ["v", v1, v2, v3] -> vec3 (parseFloat v1)  (parseFloat v2) (parseFloat v3) 
  
lineToVn line = case (String.words line) of
  ["vn", v1, v2, v3] -> vec3 (parseFloat v1)  (parseFloat v2) (parseFloat v3)
  
lineToVt line = case (String.words line) of
  ["vt", v1, v2, v3] -> vec3 (parseFloat v1)  (parseFloat v2) (parseFloat v3)
  ["vt", v1, v2] -> vec3 (parseFloat v1)  (parseFloat v2) (0.0)

lineToFace : (Array.Array Vec3, Array.Array Vec3, Array.Array Vec3) -> String -> [Triangle FaceVert]
lineToFace arrs line = case (String.words line) of
  ["f", f1, f2, f3] -> [(parseFaceVert arrs f1, parseFaceVert arrs f2, parseFaceVert arrs f3)]
  ["f", f1, f2, f3, f4] -> [(parseFaceVert arrs f1, parseFaceVert arrs f2, parseFaceVert arrs f3),
    (parseFaceVert arrs f2, parseFaceVert arrs f3, parseFaceVert arrs f4)]
  --x -> Error.raise <| show x

--Eventually will look for normals and such
--Right now, just converts the string to the vertex index
parseFaceVert : (Array.Array Vec3, Array.Array Vec3, Array.Array Vec3) -> String -> FaceVert
parseFaceVert (vArr, vtArr, vnArr) str = case String.split "//" str of
  [v, n] -> FaceVertVN <| VertVN (deIndexVert vArr v) (deIndexVert vnArr n) --vertex and normal
  [s] -> case String.split "/" s of
    [v] -> FaceVertV <| VertV (deIndexVert vArr v) --only vertex
    [v,t] -> FaceVertVT <| VertVT (deIndexVert vArr v) (deIndexVert vtArr t) --vertex and tex coords
    [v,t,n] -> FaceVertVTN <| VertVTN (deIndexVert vArr v) (deIndexVert vtArr t) (deIndexVert vnArr n) --all 3.
fromJust (Just x) = x


--Given 3 indices and vertex array, get the right vertex from the array
deIndexVert vArr s = let
    i = fromJust <| String.toInt s
  in Array.getOrFail (i-1) vArr

--Parse an OBJ file into a list of triangles  
parseObj : String -> [Triangle FaceVert]
parseObj inFile = 
  let 
    lines = String.lines inFile
    
    vLines = filter isVertexLine lines
    vertices = Array.fromList <| map lineToVert vLines
    
    vtLines = filter isVtLine lines
    texCoords = Array.fromList <| map lineToVt vtLines
    
    vnLines = filter isVnLine lines
    normals = Array.fromList <| map lineToVn vnLines
    
    fLines = filter isFaceLine lines
    faces = concat <| map (lineToFace (vertices, texCoords, normals) ) fLines
    
  in faces

--Test mesh
mesh inFile = map (\(a,b,c) -> (toVTN a, toVTN b, toVTN c)) <| parseObj inFile


--Based off the triangle rendering code from http://elm-lang.org/edit/examples/WebGL/Triangle.elm
  
-- Create the scene

--main : Signal Element

main = lift2 scene meshSig (foldp (+) 0 (fps 30))

scene : [Triangle VertVTN] -> Float -> Element
scene teapot t =
    webgl (400,400)
    [ entity vertexShader fragmentShader teapot { view = view (t / 1000), m_normal = normal (t / 1000) } ]

view : Float -> Mat4
view t =
    mul (makePerspective 45 1 0.01 100)
        (makeLookAt (vec3 (3 * cos t) 0 ((3 * sin t))) (vec3 0 0 0) (vec3 0 1 0))

normal t = transpose <| inverseOrthonormal <| view t

-- Shaders

vertexShader : Shader VertVTN { unif | view:Mat4, m_normal:Mat4 } { vcolor:Vec3 }
vertexShader = [glsl|

attribute vec3 position;
attribute vec3 texCoord;
attribute vec3 normal;
uniform mat4 view;
uniform mat4 m_normal;
varying vec3 vcolor;

void main () {
    gl_Position = view * vec4(position, 1.0);
    
    vec4 n1 = normalize(m_normal * vec4(normal, 1.0) );
    vec3 n = n1.xyz;
    //vec3 n = normal;
    
    //http://www.lighthouse3d.com/tutorials/glsl-core-tutorial/directional-lights/
    
    vec3 diffuse = vec3(1,1,0);
    
    vec3 l_dir = vec3(3,0,0);
    
    // compute the intensity as the dot product
    // the max prevents negative intensity values
    float intensity = max(dot(n, l_dir), 0.0);
 
    // Compute the color per vertex
    vcolor = intensity * diffuse;
    
    
}

|]

fragmentShader : Shader {} u { vcolor:Vec3 }
fragmentShader = [glsl|

precision mediump float;
varying vec3 vcolor;

void main () {
    gl_FragColor = vec4(vcolor, 1.0);
}

|]  

fromResponse r = case r of 
  Http.Success s -> s
  _ -> ""

inFileSig = let
    resp = Http.sendGet <| constant "resources/wt_teapot.obj"
  in lift fromResponse resp
 
meshSig = lift mesh inFileSig
