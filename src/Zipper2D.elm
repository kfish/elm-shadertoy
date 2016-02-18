module Zipper2D where

import Maybe.Extra exposing (join)

import Zipper

type alias Zipper2D a = Zipper.Zipper (Zipper.Zipper a)

empty : Zipper2D a
empty = Zipper.empty

here : Zipper2D a -> Maybe a
here z = join (Maybe.map Zipper.here (Zipper.here z))

fromLists : List (List a) -> Zipper2D a
fromLists lls = Zipper.fromList (List.map Zipper.fromList lls)

north : Zipper2D a -> Zipper2D a
north = Zipper.fwd

south : Zipper2D a -> Zipper2D a
south = Zipper.back

east : Zipper2D a -> Zipper2D a
east = Zipper.map Zipper.fwd

west : Zipper2D a -> Zipper2D a
west = Zipper.map Zipper.back

radius : Int -> Zipper2D a -> List a
radius n (Zipper.Zipper prevNS nextNS) =
    let
        r1 (Zipper.Zipper prevEW nextEW) =
            List.take n prevEW ++ List.take (n+1) nextEW
        g l = List.concatMap r1 l
    in
        g (List.take n prevNS) ++ g (List.take (n+1) nextNS)

map : (a -> b) -> Zipper2D a -> Zipper2D b
map f = Zipper.map (Zipper.map f)
