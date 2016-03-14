module Main (main) where

{-| This module drives a virtuul wurld

# Program entry
@docs main
-}

import Automaton
import Char exposing (toCode)
import Graphics.Element exposing (..)
import Maybe.Extra exposing (mapDefault)
import Random
import Set
import Signal exposing (dropRepeats, sampleOn, merge)
import Signal.Extra exposing (combine)
import String exposing (contains)
import Text
import Time exposing (Time, fps)

import WebGL exposing (..)
import Math.Matrix4 exposing (..)
import Keyboard
import Mouse
import Window

import Array2D exposing (Array2D)
import Gamepad
import Math.Procedural exposing (..)
import Model exposing (noInput)
import Engine exposing (..)
import Update
import Things.Surface2D exposing (Placement, defaultPlacement)

import Demo

import Debug

-- Pointer Lock information
port movement : Signal (Int,Int)
port isLocked : Signal Bool

-- Ability to request and exit. Click screen to request lock. Press escape to
-- give up the lock. This code can all be removed if you want to do this
-- differently.

port requestPointerLock : Signal ()
port requestPointerLock =
    let dropWhen bs def sig = Signal.map snd
            (Signal.filter fst (False, def) (Signal.map2 (,) (Signal.sampleOn sig (Signal.map not bs)) sig))
    in
    dropWhen (Signal.map2 (&&) Keyboard.shift isLocked) () Mouse.clicks


port exitPointerLock : Signal ()
port exitPointerLock =
    Signal.map (always ()) (Signal.filter (Set.member 27) Set.empty Keyboard.keysDown)

----------------------------------------------------------------------
-- Gamepad

gamepadToArrows : Gamepad.Gamepad -> { x : Float, y : Float, mx : Float, my : Float }
gamepadToArrows gamepad =
    let
        sgn x = if x < 0 then -1 else 1
        -- Apply a deadzone of size l around an axis centered at 0
        u l x = if abs x < l then 0.0 else sgn x * (abs x - l) / (1.0 - l)
        deadzone = u 0.2

{-
        -- Map a trigger to a standard range [0..1], applying a deadzone
        dc l x = let xm = (x+1.0)/2.0 in u l xm
        toTrigger = dc 0.2
-}

        axs = case gamepad.axes of
{-
                  -- Performance Designed Products Rock Candy Gamepad for Xbox 360 (Vendor: 0e6f Product: 011f)
                  [x1, y1, b1, x2, y2, b2, x3, y3] ->
                      { x = deadzone x1 / 10
                      , y = deadzone (-1.0 * y1) / 10 - toTrigger b1 + toTrigger b2
                      , mx = deadzone x2 /20
                      , my = deadzone (-1.0 * y2) / 20
                      }
-}

                  -- ©Microsoft Corporation Controller (STANDARD GAMEPAD Vendor: 045e Product: 028e)
                  [x1, y1, x2, y2] ->
                      { x = deadzone x1 / 10
                      , y = deadzone (-1.0 * y1) / 10
                      , mx= deadzone x2 / 20
                      , my= deadzone (-1.0 * y2) / 20
                      }

                  [x1,y1] ->
                      { x = x1/10, y = (-1.0 * y1)/10, mx = 0, my = 0 }

                  _ ->
                      {x = 0, y = 0, mx = 0, my = 0 }

        -- Interpret brake, accelerator as y input
        btns_y = case gamepad.buttons of
                    -- ©Microsoft Corporation Controller (STANDARD GAMEPAD Vendor: 045e Product: 028e)
                    [a, b, x, y, lt, rt, l, r, back, start, lstick, rstick, padU, padD, padL, padR, logo] -> r.value - l.value

{-
                    -- Performance Designed Products Rock Candy Gamepad for Xbox 360 (Vendor: 0e6f Product: 011f)
                    [a, b, x, y, lt, rt, back, start, logo, lstick, rstick] -> 0
-}

                    _ -> 0

    in
        { axs | y = btns_y + axs.y }

gamepadsToArrows : List Gamepad.Gamepad -> List { x : Float, y : Float, mx : Float, my : Float }
gamepadsToArrows = List.map gamepadToArrows

gamepadNothing : Gamepad.Gamepad
gamepadNothing = { id = "", axes = [], buttons = [], mapping = "" }

toStandardGamepad : Gamepad.Gamepad -> Gamepad.Gamepad
toStandardGamepad gamepad =
    if gamepad.id `contains` "STANDARD GAMEPAD" then
        gamepad
    else
        let (axes', buttons') = case (gamepad.axes, gamepad.buttons) of
            -- Performance Designed Products Rock Candy Gamepad for Xbox 360 (Vendor: 0e6f Product: 011f)
            ( [x1, y1, b1, x2, y2, b2, x3, y3]
            , [a, b, x, y, lt, rt, back, start, logo, lstick, rstick]) ->
                let
                    toTrigger b = let b' = (b+1.0)/2.0 in
                        if b' > 0 then
                            { pressed = True, value = b' }
                        else
                            { pressed = False, value = 0 }
                    b1' = toTrigger b1
                    b2' = toTrigger b2

                    toPads ax =
                        if ax > 0 then
                            ({ pressed = True, value = ax }, { pressed = False, value = 0 })
                        else if ax < 0 then
                            ({ pressed = False, value = 0 }, { pressed = True, value = -ax })
                        else
                            ({ pressed = False, value = 0 }, { pressed = False, value = 0 })
                    (padL, padR) = toPads x3
                    (padU, padD) = toPads y3

                in
                    ( [x1, y1, x2, y2]
                    , [ a, b, x, y, lt, rt, b1', b2', back, start
                      , lstick, rstick, padU, padD, padL, padR, logo]
                    )

            _ -> (gamepad.axes, gamepad.buttons)
        in { gamepad | axes = axes', buttons = buttons' }

standardGamepads : Signal (List Gamepad.Gamepad)
standardGamepads = Signal.map (List.map toStandardGamepad) Gamepad.gamepads

persistentGamepads : Signal (List (Maybe Gamepad.Gamepad))
persistentGamepads =
    let
        extract acc i gs0 = case gs0 of
            []      -> (Nothing, List.reverse acc ++ gs0)
            (g::gs) -> if g.id == i then (Just g, List.reverse acc ++ gs)
                       else extract (g::acc) i gs

        reorder is0 gs0 = case is0 of
            []        -> List.map Just (List.sortBy getId gs0)
            (i::is) -> let (gm,gs) = extract [] i gs0 in gm :: reorder is gs

        getId g = g.id
        catMaybes = List.filterMap Basics.identity

        remap : List String -> List (Maybe Gamepad.Gamepad) -> List String
        remap ids0 gs0 = case (ids0, gs0) of
            ([], _)            -> catMaybes (List.map (Maybe.map getId) gs0)
            (is, [])           -> is
            ((i::is), (g::gs)) -> Maybe.withDefault i (Maybe.map getId g) :: remap is gs

        step gs0 is0 = let gs = reorder is0 gs0 in (gs, remap is0 gs)
        a = Automaton.hiddenState [] step
    in Automaton.run a [] standardGamepads

{- a b x y ltop rtop l r 
l tiny (back), r tiny (start)
stick l down, stick r down
4way: u d l r
Xbox-logo
-}

type alias GamepadButtons = { bA : Bool, bB : Bool, bX : Bool, bY : Bool, bBack : Bool, bStart : Bool }

gamepadButtonsNone = { bA = False, bB = False, bX = False, bY = False, bBack = False, bStart = False }

gamepadToButtons : Gamepad.Gamepad -> GamepadButtons
gamepadToButtons gamepad =
    case gamepad.buttons of
        (a::b::x::y::lt::rt::l::r::back::start::_) ->
            { bA = a.pressed, bB = b.pressed, bX = x.pressed, bY = y.pressed,
              bBack = back.pressed, bStart = start.pressed }

        _ -> gamepadButtonsNone

gamepadsToButtons : List Gamepad.Gamepad -> List GamepadButtons
gamepadsToButtons = List.map gamepadToButtons

----------------------------------------------------------------------

gamepadToInputs : Time -> Gamepad.Gamepad -> Model.Inputs
gamepadToInputs dt gamepad =
    let {x,y,mx,my} = gamepadToArrows gamepad
        {bA, bB, bX, bY, bBack, bStart} = gamepadToButtons gamepad
    in  { noInput | reset = bStart, x = x, y = y, mx=mx, my=my, button_X = bX, dt = dt }

gamepadsToInputs : List (Maybe Gamepad.Gamepad) -> Time -> List Model.Inputs
gamepadsToInputs gamepads dt = List.map (mapDefault noInput (gamepadToInputs dt)) gamepads

mouseDeltas : Signal (Time, (Float, Float))
mouseDeltas =
    let step (t,(mx,my)) pt = ((Time.inSeconds t - pt, (toFloat mx, toFloat my)), t)
        a = Automaton.hiddenState 0 step
    in Automaton.run a (0, (0,0)) (Time.timestamp movement)

-- Set up 3D world
kbMouseInputs : Signal Model.Inputs
kbMouseInputs =
  let dt = Signal.map Time.inSeconds (fps 60)
      dirKeys = merge Keyboard.arrows Keyboard.wasd
      yo x = x / 500
      handleKeys s bx {x,y} kdt = dbg_X
          { noInput | isJumping = s, button_X = bx, x = toFloat x, y = toFloat y, dt=kdt }
      dbg_X i = if i.button_X then Debug.log "Button X pressed" i else i
      x_key = Keyboard.isDown 88
  in  merge
          (sampleOn dt <| Signal.map4 handleKeys Keyboard.space x_key dirKeys dt)
          (Signal.map (\(mt, (mx,my)) -> { noInput | mx= yo mx, my= yo my, mt = mt }) mouseDeltas)

gamepadInputs : Signal (List Model.Inputs)
gamepadInputs =
  let dt = Signal.map Time.inSeconds (fps 60)
  in  sampleOn dt <| Signal.map2 gamepadsToInputs persistentGamepads dt

atWithDefault : a -> Int -> List a -> a
atWithDefault def n list = Maybe.withDefault def (List.head (List.drop n list))

gamepad1 : Signal Model.Inputs
-- gamepad1 = Signal.map (Maybe.withDefault noInput << List.head) gamepadInputs
gamepad1 = Signal.map (atWithDefault noInput 0) gamepadInputs

gamepad2 : Signal Model.Inputs
gamepad2 = Signal.map (atWithDefault noInput 1) gamepadInputs

-- inputs : Signal Model.Inputs
-- inputs = merge (dropRepeats kbMouseInputs) (dropRepeats gamepad1)

inputs1 : Signal Model.Inputs
inputs1 = merge (dropRepeats kbMouseInputs) (dropRepeats gamepad1)

inputs2 : Signal Model.Inputs
inputs2 = dropRepeats gamepad2

-- person : Placement -> Array2D Float -> Signal Model.Person
-- person placement terrain = Signal.foldp (Update.step placement terrain) Model.defaultPerson inputs

person1 : Placement -> Array2D Float -> Signal Model.Person
person1 placement terrain = Signal.foldp (Update.step placement terrain) Model.defaultPerson inputs1

person2 : Placement -> Array2D Float -> Signal Model.Person
person2 placement terrain = Signal.foldp (Update.step placement terrain) Model.defaultPerson inputs2

{-| The main function -}
main : Signal Element
main = world Demo.demoThings

world : (Array2D Float -> Signal (List Thing)) -> Signal Element
world thingsOnTerrain =
  let t = Signal.foldp (+) 0 (fps 30)
      wh  = Window.dimensions
      wh2 = Signal.map (\(w,h) -> (w//2, h)) wh

      placement = defaultPlacement

      seed0 = Random.initialSeed 7777
      (terrain, seed1) = Random.generate (randTerrain2D (placement.bigSide+1)) seed0
      entities = thingsOnTerrain terrain

      oneScene = Signal.map4 scene entities wh t (person1 placement terrain)
      dualScene =
            (Signal.map2 beside
                (Signal.map4 scene entities wh2 t (person1 placement terrain))
                (Signal.map4 scene entities wh2 t (person2 placement terrain)))

      ifElse : (a -> Bool) -> b -> b -> a -> b
      ifElse p ifBranch elseBranch x = if p x then ifBranch else elseBranch
      chooseScene = Signal.map3 (ifElse (\l -> List.length l > 1)) dualScene oneScene persistentGamepads
  in 
      -- Signal.map3 lockMessage wh isLocked
      Signal.map2 debugLayer
          (combine [Signal.map show Gamepad.gamepads, Signal.map show gamepadInputs])
            chooseScene

debugLayer : List Element -> Element -> Element
debugLayer xs e = layers [ e, flow down xs ]

lockMessage : (Int,Int) -> Bool -> Element -> Element
lockMessage (w,h) isLocked e =
    layers [ e
           , container w 140 (midLeftAt (absolute 40) (relative 0.5))
                 (if isLocked then exitMsg else enterMsg)
           ]

enterMsg : Element
enterMsg = message "Click to go full screen and move your head with the mouse."

exitMsg : Element
exitMsg = message "Press <escape> to exit full screen."

message : String -> Element
message msg =
   leftAligned <| Text.monospace <| Text.fromString <|
    "Use gamepad, arrows or WASD keys to move.\n\n" ++ msg
