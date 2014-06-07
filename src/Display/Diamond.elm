module Display.Diamond (fogMountainsDiamond, diamond) where

import Math.Vector2 (Vec2)
import Math.Vector3 (..)
import Math.Matrix4 (..)
import Graphics.WebGL (..)

import Shaders.Fire (fire)
import Shaders.FogMountains (fogMountains)
--import Shaders.SimplePlasma (simplePlasma)
--import Shaders.VoronoiDistances (voronoiDistances)
import Shaders.WorldVertex (Vertex, worldVertex)

import Model

fogMountainsDiamond : (Int,Int) -> Time -> Mat4 -> Entity
fogMountainsDiamond = diamond worldVertex fogMountains

-- diamond : Shader attributes uniforms varying -> Shader {} uniforms varyings
--    -> (Int,Int) -> Time -> Mat4 -> Entity
diamond vertexShader fragmentShader (w,h) t view =
    let resolution = vec3 (toFloat w) (toFloat h) 0
        s = inSeconds t
    in
        entity vertexShader fragmentShader diamondMesh
            { iResolution=resolution, iGlobalTime=s, view=view }

both : (Vec3 -> Vec3) -> Vertex -> Vertex
both f x = { position = f x.position, coord = f x.coord }

unfold : Int -> (a -> a) -> a -> [a]
unfold n f x = if n==0 then [] else
  let res=f x in (res :: unfold (n-1) f res)

zip3 : [a] -> [b] -> [c] -> [(a,b,c)]
zip3 xs ys zs =
  case (xs, ys, zs) of
    (x::xs', y::ys', z::zs') -> (x,y,z) :: zip3 xs' ys' zs'
    _ -> []

rot n = makeRotate (2*pi/n) (vec3 0 0 1)

seven : Vertex -> [Vertex]
seven = unfold 7 (both (transform (rot 8)))

eights x = let x7 = seven x in (x::x7, x7++[x])

--diamondMesh : [Triangle { pos:Vec3, coord:Vec3 }]
diamondMesh : [Triangle Vertex]
diamondMesh =
  let
      -- Vertices
      table0 = { position = vec3 0 0 0, coord = vec3 0 0 0 }
      tableV = { position = vec3 0.57 0 0, coord = vec3 0.57 0 0 }
      (tableVS0, tableVS1) = eights tableV

      facetZ = -0.2
      facet0 = both (transform (rot -16)) { position = vec3 0.8 0 facetZ, coord = vec3 0.8 0.2 0 }
      (facetVS0, facetVS1) = eights facet0

      girdleZ = -0.5
      girdleT0 = { position = vec3 1 0 girdleZ, coord = vec3 0.9 0.3 0 }
      (girdleTS0, girdleTS1) = eights girdleT0
      girdleF0 = both (transform (rot 16)) girdleT0
      girdleFS = girdleF0 :: seven girdleF0

      pavilionZ = -1.3
      pavilionT0 = { position = vec3 0.2 0 pavilionZ, coord = vec3 1.3 0.4 0 }
      pavilionF0 = both (transform (rot -16)) pavilionT0
      (pavilionVS0, pavilionVS1) = eights pavilionF0

      cutlet = { position = vec3 0 0 -1.6, coord = vec3 0 0.41 0 }

      -- Triangles
      mkTable v1 v2 = (table0, v1, v2)
      table = zipWith mkTable tableVS1 tableVS0

      stars = zip3 tableVS0 tableVS1 facetVS1

      bezelL = zip3 facetVS0 tableVS0 girdleTS0
      bezelR = zip3 facetVS1 girdleTS0 tableVS0

      upperGirdleL = zip3 girdleTS0 facetVS1 girdleFS
      upperGirdleR = zip3 girdleFS facetVS1 girdleTS1

      lowerGirdleL = zip3 girdleTS0 girdleFS pavilionVS1
      lowerGirdleR = zip3 girdleFS pavilionVS1 girdleTS1

      pavilionFacetL = zip3 pavilionVS0 girdleTS0 (repeat 8 cutlet)
      pavilionFacetR = zip3 girdleTS0 pavilionVS1 (repeat 8 cutlet)
      
  in
      table ++
      stars ++
      bezelL ++ bezelR ++
      upperGirdleL ++ upperGirdleR ++
      lowerGirdleL ++ lowerGirdleR ++
      pavilionFacetL ++ pavilionFacetR ++
      []

