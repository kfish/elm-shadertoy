module Demo (demoThings) where

import Engine (..)

import Things.Ground (ground)
import Things.Cube (cloudsCube, fireCube, fogMountainsCube, plasmaCube, voronoiCube)
import Things.Diamond (cloudsDiamond, fogMountainsDiamond)
import Things.Capsule (capsule)
-- import Things.Teapot (teapot)

demoThings : Signal Things
demoThings = gather [
    ground,
    -- place   0   3   0 <~ teapot,
    place   2   1   -1 <~ capsule,
    place   5 1.5   1 <~ cloudsDiamond,
    place  10   0  10 <~ voronoiCube,
    place -10   0 -10 <~ fireCube,
    place  10 1.5 -10 <~ fogMountainsCube
    ]
