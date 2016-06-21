module Display.Diamond exposing (cloudsDiamond, fogMountainsDiamond, diamond)

import List exposing (map2, repeat)
import Time exposing (Time, inSeconds)

import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import WebGL exposing (..)

import Shaders.Clouds exposing (clouds)
import Shaders.Fire exposing (fire)
import Shaders.FogMountains exposing (fogMountains)
--import Shaders.SimplePlasma exposing (simplePlasma)
--import Shaders.VoronoiDistances exposing (voronoiDistances)
import Shaders.WorldVertex exposing (Vertex, worldVertex)

import Model

type alias Triangle a = (a,a,a)
type alias Vertex = { pos:Vec3, coord:Vec3 }

cloudsDiamond : (Int,Int) -> Time -> Mat4 -> Renderable
cloudsDiamond = diamond worldVertex clouds

fogMountainsDiamond : (Int,Int) -> Time -> Mat4 -> Renderable
fogMountainsDiamond = diamond worldVertex fogMountains

-- diamond : Shader attributes uniforms varying -> Shader {} uniforms varyings
--    -> (Int,Int) -> Time -> Mat4 -> Renderable
diamond vertexShader fragmentShader (w,h) t view =
    let resolution = vec3 (toFloat w) (toFloat h) 0
        s = inSeconds t
    in
        render vertexShader fragmentShader diamondMesh
            { iResolution=resolution, iGlobalTime=s, view=view }

unfold : Int -> (a -> a) -> a -> List a
unfold n f x = if n==0 then [] else
  let res=f x in (res :: unfold (n-1) f res)

zip3 : List a -> List b -> List c -> List (a,b,c)
zip3 xs ys zs =
  case (xs, ys, zs) of
    (x::xs', y::ys', z::zs') -> (x,y,z) :: zip3 xs' ys' zs'
    _ -> []

rotY n = makeRotate (2*pi/n) (vec3 0 1 0)
rotZ n = makeRotate (-2*pi/n) (vec3 0 0 1)

rotBoth : Float -> Vertex -> Vertex
rotBoth n x = { pos = transform (rotY n) x.pos, coord = transform (rotZ n) x.coord }

seven : Vertex -> List Vertex
seven = unfold 7 (rotBoth 8)

eights x = let x7 = seven x in (x::x7, x7++[x])

diamondMesh : Drawable Vertex
diamondMesh =
  let
      yOffset = 1.21
      yMul = -4.2
      -- Vertices
      table0 = { pos = vec3 0 0 0, coord = vec3 0 (yMul*(0.0-yOffset)) 0 }
      tableV = { pos = vec3 0.57 0 0, coord = vec3 0 (yMul*(0.57-yOffset)) 0 }
      (tableVS0, tableVS1) = eights tableV

      facetY = -0.2
      facet0 = rotBoth -16 { pos = vec3 0.8 facetY 0, coord = vec3 0.2 (yMul*(0.8-yOffset)) 0 }
      (facetVS0, facetVS1) = eights facet0

      girdleY = -0.5
      girdleT0 = { pos = vec3 1 girdleY 0, coord = vec3 0.3 (yMul*(0.9-yOffset)) 0 }
      (girdleTS0, girdleTS1) = eights girdleT0
      girdleF0 = rotBoth 16 girdleT0
      girdleFS = girdleF0 :: seven girdleF0

      pavilionY = -1.3
      pavilionT0 = { pos = vec3 0.2 pavilionY 0, coord = vec3 0.4 (yMul*(1.3-yOffset)) 0 }
      pavilionF0 = rotBoth -16 pavilionT0
      (pavilionVS0, pavilionVS1) = eights pavilionF0

      cutlet = { pos = vec3 0 -1.6 0, coord = vec3 0.41 (yMul*(0.87-yOffset)) 0 }

      -- Triangles
      mkTable v1 v2 = (table0, v1, v2)
      table = map2 mkTable tableVS1 tableVS0

      stars = zip3 tableVS0 tableVS1 facetVS1

      bezelL = zip3 facetVS0 tableVS0 girdleTS0
      bezelR = zip3 facetVS1 girdleTS0 tableVS0

      upperGirdleL = zip3 girdleTS0 facetVS1 girdleFS
      upperGirdleR = zip3 girdleFS facetVS1 girdleTS1

      lowerGirdleL = zip3 girdleTS0 girdleFS pavilionVS1
      lowerGirdleR = zip3 girdleFS pavilionVS1 girdleTS1

      pavilionFacetL = zip3 pavilionVS0 girdleTS0 (repeat 8 cutlet)
      pavilionFacetR = zip3 girdleTS0 pavilionVS1 (repeat 8 cutlet)
      
  in Triangle <|
      table ++
      stars ++
      bezelL ++ bezelR ++
      upperGirdleL ++ upperGirdleR ++
      lowerGirdleL ++ lowerGirdleR ++
      pavilionFacetL ++ pavilionFacetR ++
      []

