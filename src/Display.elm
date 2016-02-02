module Display (scene, crateEntities) where

import Color exposing (rgb)
import Graphics.Element exposing (..)
import Text
import Time exposing (Time)

-- import Http exposing (..)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import WebGL exposing (..)

import Model
import Display.World exposing (ground)
import Display.Crate exposing (cloudsCube, fireCube, fogMountainsCube, plasmaCube, voronoiCube)
import Display.Diamond exposing (cloudsDiamond, fogMountainsDiamond)

import Shaders.WorldVertex exposing (Vertex, worldVertex)
import Shaders.Fire exposing (fire)
import Shaders.SimplePlasma exposing (simplePlasma)
import Shaders.VoronoiDistances exposing (voronoiDistances)

view : (Int,Int) -> Model.Person -> Mat4
view (w,h) person =
    mul (makePerspective 45 (toFloat w / toFloat h) 0.01 100)
        (makeLookAt person.position (person.position `add` Model.direction person) j)

scene : ((Int,Int) -> Time -> Mat4 -> List Renderable)
    -> (Int,Int) -> Time -> Bool -> Model.Person -> Element
scene entities (w,h) t isLocked person =
    layers [ color (rgb 135 206 235) (spacer w h)
           , webgl (w,h) (entities (w,h) t (view (w,h) person))
           , container w 140 (midLeftAt (absolute 40) (relative 0.5))
                 (if isLocked then exitMsg else enterMsg)
           ]

crateEntities : (Int,Int) -> Time -> Mat4 -> List Renderable
crateEntities resolution t view =
    let cubes = 
            [ fogMountainsDiamond  resolution t (translate3 0 1.5 0 view)
            , cloudsDiamond  resolution t (translate3 5 1.5 1 view)
            , voronoiCube resolution t (translate3  10 0  10 view)
            , fireCube    resolution t (translate3 -10 0 -10 view)
            , fogMountainsCube resolution t (translate3 10 1.5 -10 view)
            ]
    in  
        ground view :: cubes

enterMsg : Element
enterMsg = message "Click to go full screen and move your head with the mouse."

exitMsg : Element
exitMsg = message "Press <escape> to exit full screen."

message : String -> Element
message msg =
    leftAligned <| Text.monospace <| Text.fromString <|
    "This uses stuff that is only available in Chrome and Firefox!\n" ++
    "\nPress arrows or WASD keys to move, space bar to jump.\n\n" ++ msg
