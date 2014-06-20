module Display.Diamond (cloudsDiamond, fogMountainsDiamond, diamond) where

import Math.Vector2 (Vec2)
import Math.Vector3 (..)
import Math.Matrix4 (..)
import Graphics.WebGL (..)

import Shaders.Clouds (clouds)
import Shaders.Fire (fire)
import Shaders.FogMountains (fogMountains)
--import Shaders.SimplePlasma (simplePlasma)
--import Shaders.VoronoiDistances (voronoiDistances)
import Shaders.WorldVertex (Vertex, worldVertex)

import Model
import Engine (..)

-- cloudsDiamond : (Int,Int) -> Time -> Mat4 -> Entity
cloudsDiamond : Perception -> Entity
cloudsDiamond = diamond worldVertex clouds

-- fogMountainsDiamond : (Int,Int) -> Time -> Mat4 -> Entity
fogMountainsDiamond : Perception -> Entity
fogMountainsDiamond = diamond worldVertex fogMountains

-- type ShadertoyUniforms a = { a | iResolution : Vec3, iGlobalTime : Float, view : (Int,Int) }

-- diamond : Shader attributes uniforms varying -> Shader {} uniforms varyings
--    -> (Int,Int) -> Time -> Mat4 -> Entity
-- diamond : Shader attributes (ShadertoyUniforms {}) varyings -> Shader {} (ShadertoyUniforms {})  varyings -> Perception -> Entity
diamond vertexShader fragmentShader p =
    let (w,h) = p.resolution
        resolution = vec3 (toFloat w) (toFloat h) 0
        s = inSeconds p.globalTime
    in
        entity vertexShader fragmentShader diamondMesh
            { iResolution=resolution, iGlobalTime=s, view=p.viewMatrix }

unfold : Int -> (a -> a) -> a -> [a]
unfold n f x = if n==0 then [] else
  let res=f x in (res :: unfold (n-1) f res)

zip3 : [a] -> [b] -> [c] -> [(a,b,c)]
zip3 xs ys zs =
  case (xs, ys, zs) of
    (x::xs', y::ys', z::zs') -> (x,y,z) :: zip3 xs' ys' zs'
    _ -> []

rotY n = makeRotate (2*pi/n) (vec3 0 1 0)
rotZ n = makeRotate (-2*pi/n) (vec3 0 0 1)

rotBoth : Float -> Vertex -> Vertex
rotBoth n x = { position = transform (rotY n) x.position, coord = transform (rotZ n) x.coord }

seven : Vertex -> [Vertex]
seven = unfold 7 (rotBoth 8)

eights x = let x7 = seven x in (x::x7, x7++[x])

--diamondMesh : [Triangle { pos:Vec3, coord:Vec3 }]
diamondMesh : [Triangle Vertex]
diamondMesh =
  let
      yOffset = 1.21
      yMul = -4.2
      -- Vertices
      table0 = { position = vec3 0 0 0, coord = vec3 0 (yMul*(0.0-yOffset)) 0 }
      tableV = { position = vec3 0.57 0 0, coord = vec3 0 (yMul*(0.57-yOffset)) 0 }
      (tableVS0, tableVS1) = eights tableV

      facetY = -0.2
      facet0 = rotBoth -16 { position = vec3 0.8 facetY 0, coord = vec3 0.2 (yMul*(0.8-yOffset)) 0 }
      (facetVS0, facetVS1) = eights facet0

      girdleY = -0.5
      girdleT0 = { position = vec3 1 girdleY 0, coord = vec3 0.3 (yMul*(0.9-yOffset)) 0 }
      (girdleTS0, girdleTS1) = eights girdleT0
      girdleF0 = rotBoth 16 girdleT0
      girdleFS = girdleF0 :: seven girdleF0

      pavilionY = -1.3
      pavilionT0 = { position = vec3 0.2 pavilionY 0, coord = vec3 0.4 (yMul*(1.3-yOffset)) 0 }
      pavilionF0 = rotBoth -16 pavilionT0
      (pavilionVS0, pavilionVS1) = eights pavilionF0

      cutlet = { position = vec3 0 -1.6 0, coord = vec3 0.41 (yMul*(0.87-yOffset)) 0 }

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

