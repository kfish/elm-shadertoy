module Zipper where

type Zipper a = Zipper (List a) (List a)
{-
    { here : Maybe a
    , prev : List a
    , next : List a
    }
-}

empty : Zipper a
empty = Zipper [] []

here : Zipper a -> Maybe a
here (Zipper _ next) = List.head next

fromList : List a -> Zipper a
fromList = Zipper []

fwd : Zipper a -> Zipper a
fwd (Zipper prev next) =
    case next of
        []        -> Zipper prev next
        (x :: xs) -> Zipper (x :: prev) xs

back : Zipper a -> Zipper a
back (Zipper prev next) =
    case prev of
        []        -> Zipper prev next
        (x :: xs) -> Zipper xs (x :: next)

map : (a -> b) -> Zipper a -> Zipper b
map f (Zipper prev next) = Zipper (List.map f prev) (List.map f next)
