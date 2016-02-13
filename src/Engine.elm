module Engine where

import Color exposing (rgb)
import Graphics.Element exposing (..)
import Maybe exposing (withDefault)
import Signal exposing (foldp)
import Signal.Extra exposing ((<~), combine)
import Time exposing (Time)

import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (Mat4)
import Math.Matrix4 as M4
import WebGL exposing (..)

import Model

type alias Perception = {
    resolution : (Int, Int),
    globalTime : Time,
    viewMatrix : Mat4
}

type alias See = Perception -> List Renderable

type alias Visible a = { a | see : See }

type alias Oriented a = { a | pos : Vec3, orientation : Vec3 }
type alias Moving a = Oriented { a | velocity : Vec3 }
type alias Massive a = { a | mass : Float }
type alias Spherical a = { a | radius : Float }

folds : b -> (a -> b -> b) -> Signal b -> Signal a -> Signal b
folds dfl step state input =
    let f (b0,inputs) bm = case bm of
            Nothing -> Just b0
            Just b  -> Just (step inputs b)
    in Signal.map (withDefault dfl) (foldp f Nothing (Signal.map2 (,) state input))

-- NAMING: this name is terrible, please suggest an alternative
-- data TCont a = TCont a (Time -> a -> (a, TCont a))
-- data TCont a = TCont a (Time -> a -> TCont a)
type TCont a = TCont (Time -> a -> (TCont a, a))

foldTCont : TCont a -> a -> Signal Time -> Signal a
foldTCont c init t =
  let upd dt (TCont f, x) = f dt x
  in snd <~ foldp upd (c, init) t

identityTCont : TCont a
identityTCont = TCont (\_ x -> (identityTCont, x))

foldSigTCont : a -> Signal (TCont a) -> Signal a -> Signal Time -> Signal a
foldSigTCont dfl c init t =
  let upd dt (TCont f, x) = f dt x
  in snd <~ folds (identityTCont, dfl) upd (Signal.map2 (,) c init) t

foldSigTCont2 : a -> TCont a -> Signal a -> Signal Time -> Signal a
foldSigTCont2 dfl c init t =
  let upd dt (TCont f, x) = f dt x
  in snd <~ folds (identityTCont, dfl) upd (Signal.map2 (,) (Signal.constant c) init) t


simpleTCont : (Time -> a -> a) -> TCont a
simpleTCont step = TCont (\dt x -> (simpleTCont step, step dt x))

tcAndThen : TCont a -> TCont a -> TCont a
tcAndThen (TCont f1) (TCont f2) =
  TCont <| \dt x ->
    let (f1', x') = f1 dt x
        (f2', x'') = f2 dt x'
    in (tcAndThen f1' f2', x'')

fAndThen : (Time -> a -> a) -> (Time -> a -> a) -> (Time -> a -> a)
fAndThen f g = \dt -> f dt >> g dt

composeTCont : (Time -> a -> a) -> (Time -> a -> a) -> TCont a
composeTCont f g = simpleTCont (fAndThen f g)

{- 
Just a sketch ... use Automaton :)

type DCont f a = DCont (Time -> a -> DCont f a) a

return (DCont _ a) = a

-- pair : DCont f a -> DCont g b -> DCont (f,g) (a,b)

foldDCont : DCont f a -> Signal Time -> Signal a
foldDCont dcont t =
  let upd dt (DCont g x) = g dt x
  in Signal.map return (foldp upd dcont t)
-}

{-
tcAndThen : a -> TCont a -> TCont a -> TCont a
tcAndThen cur (TCont f1) (TCont f2) =
  let step dt x =
    let (TCont f1', x1') = f1 dt x
        (TCont f2', x2') = f2 dt x1'
    in  tcAndThen x2' (TCont x1' f1') (TCont x2' f2')
  in TCont cur step
-}

{-
tcAndThenSig : Signal a -> (Signal a -> Signal (TCont a)) -> (Signal a -> Signal (TCont a)) -> Signal (TCont a)
tcAndThenSig init c1 c2 = lift3 tcAndThen init (c1 init) (c2 init)
-}


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


type Thing = Thing Vec3 Vec3 See
{-
    pos : Vec3,
    orientation : Vec3,
    see : See
-} 

-- Really, all we want to be able to do is get a snapshot
-- of each object's position and See function etc., so perhaps they
-- should just contain a function that gives us exactly that
extractThing : Oriented (Visible a) -> Thing
extractThing x =
{-
    let
        -- xo = { x - orientation }
        -- xp = { xo - pos }
        -- o = x.orientation xp
    in Thing x.pos o x.see
-}
    -- Thing x.pos (vec3 1 0 1) x.see
    Thing x.pos x.orientation x.see

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

mapApply : List (a -> List b) -> a -> List b
mapApply fs x = List.concat <| List.map (\f -> f x) fs

gather : List (Signal (List a)) -> Signal (List a)
gather = Signal.map List.concat << combine

tview : (Mat4 -> Mat4) -> See -> See
tview f see p = see { p | viewMatrix = f p.viewMatrix }

place : Float -> Float -> Float -> Thing -> Thing
place x y z (Thing _ o s) = Thing (vec3 x y z) o s

orient : Thing -> See
orient (Thing position orientation see) =
    let z_axis = vec3 0 0 1
        rot_angle = 0 - acos (dot orientation z_axis)
        rot_axis = normalize (cross orientation z_axis)
    in
        tview (M4.translate position) << tview (M4.rotate rot_angle rot_axis) <| see


look : (Int,Int) -> Model.Person -> Mat4
look (w,h) person =
    M4.mul (M4.makePerspective 45 (toFloat w / toFloat h) 0.01 100)
           (M4.makeLookAt person.pos (person.pos `add` Model.direction person) j)

scene : List Thing -> (Int,Int) -> Time -> Model.Person -> Element
scene things (w,h) t person =
  let
    see = mapApply (List.map orient things)
    p = { viewMatrix = look (w,h) person, globalTime = t, resolution = (w,h) }
  in
    layers [ color (rgb 135 206 235) (spacer w h)
           , webgl (w,h) (see p)
           ]

