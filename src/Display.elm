module Display (scene, ourEntities) where

import Http (..)
import Math.Vector2 (Vec2)
import Math.Vector3 (..)
import Math.Matrix4 (..)
import Graphics.WebGL (..)

import Model
import Engine (..)

import Display.World (ground)
import Display.Crate (cloudsCube, fireCube, fogMountainsCube, plasmaCube, voronoiCube)
import Display.Diamond (cloudsDiamond, fogMountainsDiamond)
import Display.Obj (teapotSig)

import Shaders.WorldVertex (Vertex, worldVertex)
import Shaders.Fire (fire)
import Shaders.SimplePlasma (simplePlasma)
import Shaders.VoronoiDistances (voronoiDistances)

view : (Int,Int) -> Model.Person -> Mat4
view (w,h) person =
    mul (makePerspective 45 (toFloat w / toFloat h) 0.01 100)
        (makeLookAt person.position (person.position `add` Model.direction person) j)

scene : (Perception -> [Entity])
    -> (Int,Int) -> Time -> Bool -> Model.Person -> Element
scene entities (w,h) t isLocked person =
  let
    p = { viewMatrix = view (w,h) person, globalTime = t, resolution = (w,h) }
  in
    layers [ color (rgb 135 206 235) (spacer w h)
           , webgl (w,h) (entities p)
           , container w 140 (midLeftAt (absolute 40) (relative 0.5))
                 (if isLocked then exitMsg else enterMsg)
           ]

mapApply : [(a -> b)] -> a -> [b]
mapApply fs x = map (\f -> f x) fs

gather : [Signal (a -> b)] -> Signal (a -> [b])
gather = lift mapApply . combine

ourEntities : Signal (Perception -> [Entity])
ourEntities = gather [
    groundSig, teapotSig,
    cloudsDiamondSig, voronoiCubesSig,
    fireCubeSig, fogMountainsCubeSig ]

groundSig : Signal (Perception -> Entity)
groundSig = constant (\p -> ground p.viewMatrix)

place : (Perception -> Entity) -> Float -> Float -> Float -> Perception -> Entity
place obj x y z p = obj { p | viewMatrix <- translate3 x y z p.viewMatrix }

cloudsDiamondSig : Signal (Perception -> Entity)
cloudsDiamondSig = constant <| place cloudsDiamond 5 1.5 1

voronoiCubesSig = constant <| place voronoiCube 10 0 10

fireCubeSig = constant <| place fireCube -10 0 -10

fogMountainsCubeSig = constant <| place fogMountainsCube 10 1.5 -10

enterMsg : Element
enterMsg = message "Click to go full screen and move your head with the mouse."

exitMsg : Element
exitMsg = message "Press <escape> to exit full screen."

message : String -> Element
message msg =
   plainText <|
    "This uses stuff that is only available in Chrome and Firefox!\n" ++
    "\nPress arrows or WASD keys to move, space bar to jump.\n\n" ++ msg
