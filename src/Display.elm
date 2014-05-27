module Display (scene) where

import Http (..)
import Math.Vector2 (Vec2)
import Math.Vector3 (..)
import Math.Matrix4 (..)
import Graphics.WebGL (..)

import Model
import Display.World (ground)
import Display.Crate (crate)

view : (Int,Int) -> Model.Person -> Mat4
view (w,h) person =
    mul (makePerspective 45 (toFloat w / toFloat h) 0.01 100)
        (makeLookAt person.position (person.position `add` Model.direction person) j)

scene : (Int,Int) -> Time -> Bool -> Model.Person -> Element
scene (w,h) t isLocked person =
    layers [ color (rgb 135 206 235) (spacer w h)
           , asText (inSeconds t)
           , webgl (w,h) (entities (w,h) t (view (w,h) person))
           , container w 140 (midLeftAt (absolute 40) (relative 0.5)) . plainText <|
               "This uses stuff that is only available in Chrome and Firefox!\n\nWASD keys to move, space bar to jump.\n\n" ++
               if isLocked
                  then "Press <escape> to exit full screen."
                  else "Click to go full screen and move your head with the mouse."
           ]

entities : (Int,Int) -> Time -> Mat4 -> [Entity]
entities resolution t view =
    let crates = 
            [ crate resolution t view
            , crate resolution t (translate3  10 0  10 view)
            , crate resolution t (translate3 -10 0 -10 view)
            ]
    in  
        ground view :: crates
