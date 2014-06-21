module Demo (demoThings) where

import Graphics.WebGL (..)

import Engine (..)

import Things.Ground (ground)
import Things.Cube (cloudsCube, fireCube, fogMountainsCube, plasmaCube, voronoiCube)
import Things.Diamond (cloudsDiamond, fogMountainsDiamond)
import Things.Teapot (teapot)


demoThings : Signal (Perception -> [Entity])
demoThings = gather [
    groundSig, t2,
    cloudsDiamondSig, voronoiCubesSig,
    fireCubeSig, fogMountainsCubeSig ]

t2 = place 0 3 0 <~ teapot

groundSig : Signal (Perception -> Entity)
groundSig = constant (\p -> ground p.viewMatrix)

cloudsDiamondSig : Signal (Perception -> Entity)
cloudsDiamondSig = constant <| place 5 1.5 1 cloudsDiamond

voronoiCubesSig = constant <| place 10 0 10 voronoiCube

fireCubeSig = constant <| place -10 0 -10 fireCube

fogMountainsCubeSig = constant <| place 10 1.5 -10 fogMountainsCube

