module Engine where

import Math.Vector3 (..)
import Math.Matrix4 (..)
import Graphics.WebGL (..)

import Model

type Perception = {
    resolution : (Int, Int),
    globalTime : Time,
    viewMatrix : Mat4
}

type Thing = (Perception -> [Entity])

{-

-- TODO: Obstructions: tag with a time, take only newest obstruction?
actualObstruction = take 1 . sortBy time . catMaybes . map .obstruct things

type Thing = {
    obstruct : Motion -> Maybe Obstruction,
    perceive : Perception -> [Entity]
}

type Motion = {
    displacement : Vec3,
    mass? velocity?
}

data Obstruction =
    Obstruct Vec3 -- player stops at pos
  | Bounce Vec3 Vec3 -- player stops at pos, new velocity
  | Hyperlink URL -- new href

typical implementation:

fooThing : Signal Thing
fooThing = lift2 Thing fooObstruct fooPerceive

fooObstruct : Motion -> Maybe Obstruction
fooObstruct move = ...

fooPerceive : Perception -> [Entity]
fooPerceive p = ...

-}

mapApply : [(a -> [b])] -> a -> [b]
mapApply fs x = concat <| map (\f -> f x) fs

gather : [ Signal [(a -> [b])] ] -> Signal (a -> [b])
gather = lift (mapApply . concat) . combine

tview : (Mat4 -> Mat4) -> Thing -> Thing
tview f obj p = obj { p | viewMatrix <- f p.viewMatrix }

place : Float -> Float -> Float -> Thing -> Thing
place x y z = tview (translate3 x y z)

orient : { r | thing:Thing, position:Vec3, orientation:Vec3 } -> Thing
orient o =
    let z_axis = vec3 0 0 1
        rot_angle = 0 - acos (dot o.orientation z_axis)
        rot_axis = normalize (cross o.orientation z_axis)
    in
        tview (translate o.position) . tview (rotate rot_angle rot_axis) <| o.thing


look : (Int,Int) -> Model.Person -> Mat4
look (w,h) person =
    mul (makePerspective 45 (toFloat w / toFloat h) 0.01 100)
        (makeLookAt person.position (person.position `add` Model.direction person) j)

scene : (Perception -> [Entity])
    -> (Int,Int) -> Time -> Model.Person -> Element
scene entities (w,h) t person =
  let
    p = { viewMatrix = look (w,h) person, globalTime = t, resolution = (w,h) }
  in
    layers [ color (rgb 135 206 235) (spacer w h)
           , webgl (w,h) (entities p)
           ]

