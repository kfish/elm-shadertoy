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

type See = Perception -> [Entity]

type Visible a = { a | see : See }

type Oriented a = { a | position : Vec3, orientation : a -> Vec3 }
type Moving a = Oriented { a | velocity : Vec3 }
type Massive a = { a | mass : Float }
type Spherical a = { a | radius : Float }

folds : b -> (a -> b -> b) -> Signal b -> Signal a -> Signal b
folds dfl step state input =
    let f g (b0,is) bm = case bm of
            Nothing -> Just b0
            Just b -> Just (g is b)
    in maybe dfl id <~ foldp (f step) Nothing (lift2 (,) state input)

-- NAMING: this name is terrible, please suggest an alternative
-- data TCont a = TCont a (Time -> a -> (a, TCont a))
data TCont a = TCont a (Time -> a -> TCont a)

-- foldTCont : TCont a -> Signal Time -> Signal a
foldTCont c t =
  let upd dt (TCont x f) = f dt x
      get (TCont x _) = x
  in get <~ foldp upd c t

-- foldSigTCont : Signal (TCont a) -> Signal Time -> Signal a
foldSigTCont dfl c t =
  let upd dt (TCont x f) = f dt x
      get (TCont x _) = x
  in get <~ folds dfl upd c t

simpleTCont : (Time -> a -> a) -> a -> TCont a
simpleTCont step init =
  let upd dt x = simpleTCont step (step dt x)
  in TCont init upd

{-
signalTCont : a -> (a -> TCont a) -> Signal a -> Signal a
signalTCont dfl mk init =
    let
    in maybe (mk dfl) id <~ 
-}

{-

-- Use Oleg-style type-hiding to hide the actual details of a Thing inside its
movement continuation

eg. the private data for a thing is of a, then:

data Cont = Cont (Time -> [SelfMoving] -> ([SelfMoving], Cont))

moveSelf : a -> Cont
moveSelf priv dt bs =
    let
        -- do stuff 
        priv' = ... -- calculate updated priv
        bs' = ...  -- calculate updated bs
    in (bs', moveSelf priv')
    
Then the next round, we just call the continuation function

-}

data Thing = Thing Vec3 Vec3 See
{-
    position : Vec3,
    orientation : Vec3,
    see : See
-} 

extractThing : Oriented (Visible a) -> Thing
extractThing x =
    let
        xo = { x - orientation }
        xp = { xo - position }
        o = x.orientation xp
    in Thing x.position o x.see

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

gather : [ Signal [a] ] -> Signal [a]
gather = lift concat . combine

tview : (Mat4 -> Mat4) -> See -> See
tview f see p = see { p | viewMatrix <- f p.viewMatrix }

place : Float -> Float -> Float -> Thing -> Thing
place x y z (Thing _ o s) = Thing (vec3 x y z) o s

orient : Thing -> See
orient (Thing position orientation see) =
    let z_axis = vec3 0 0 1
        rot_angle = 0 - acos (dot orientation z_axis)
        rot_axis = normalize (cross orientation z_axis)
    in
        tview (translate position) . tview (rotate rot_angle rot_axis) <| see


look : (Int,Int) -> Model.Person -> Mat4
look (w,h) person =
    mul (makePerspective 45 (toFloat w / toFloat h) 0.01 100)
        (makeLookAt person.position (person.position `add` Model.direction person) j)

scene : [Thing] -> (Int,Int) -> Time -> Model.Person -> Element
scene things (w,h) t person =
  let
    see = mapApply (map orient things)
    p = { viewMatrix = look (w,h) person, globalTime = t, resolution = (w,h) }
  in
    layers [ color (rgb 135 206 235) (spacer w h)
           , webgl (w,h) (see p)
           ]

