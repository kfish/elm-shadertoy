module ObjTest where

import Graphics.ObjParser (..)
import Graphics.ObjTypes (..)

import LoadAssets as Load

-- import Graphics.Camera as Camera

import Math.Vector3 (..)
import Math.Matrix4 (..)
import Graphics.WebGL (..)

import Http



--Based off the triangle rendering code from http://elm-lang.org/edit/examples/WebGL/Triangle.elm
  
-- Create the scene


-- camera =  foldp Camera.step Camera.defaultCamera Camera.inputs

--main : Signal Element

--loadObj : Text -> Text -> Signal Thing
loadObjj objFile texFile0 = let
    inResp = Http.sendGet <| constant objFile -- "/capsule.obj"
    texResp = loadTexture texFile0 -- "/capsule0.jpg"
    inAsset = lift Load.toAsset inResp
    texAsset = lift Load.toAsset inResp
    assets = combine [inAsset, texAsset]
    loadStatSig = lift Load.toStatus assets

    modelSig = lift3
     (\loadStatSig inFile texFile -> case loadStatSig of
        Load.Complete -> let
          tex = Load.fromResponseOrFail texFile
          material = {
            baseColor = OneColor (vec3 0 0 0), -- TexColor tex,
            diffuseColor = Nothing,
            specColor = Nothing,
            specCoeff = Nothing,
            bumpMap = Nothing,
            reflectivity = Nothing }
          
        in  toModel (Load.fromResponseOrFail inFile) ( FullMaterial material tex )
        _ -> emptyModel
     ) loadStatSig inResp texResp
    
  
    objProperties = {position = vec3 0 0 0,
                rotation = 0,
                scaleFactor = vec3 0 0 0}
  
  in lift (\p s -> render s (myUnis p)) modelSig
-- lift2 (\p -> render modelSig (myUnis p))
-- lift2 render modelSig myUnis
  
render model unis = toEntity model unis

myUnis p = uniformsAtTime p.globalTime p.viewMatrix

uniformsAtTime t v = let
    m = modelMat (t / 1500)
    -- v = view dims cam
  in { viewMatrix = v,  modelMatrix = m}

{-
--Adapted from firstPerson example
view : (Int,Int) -> Camera.Camera -> Mat4  
view (w,h) cam = 
    mul (makePerspective 45 (toFloat w / toFloat h) 0.01 100)
        (makeLookAt cam.position (cam.position `add` Camera.direction cam) j)  
-}

modelMat : Float -> Mat4
modelMat t = let
    s = identity
    tr = identity
    r = makeRotate t (vec3 0 1 0)
  in mul tr (mul r s)
