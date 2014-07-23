module Physics.Drop where

import Array
import Math.Vector3 as V3
import Math.Vector3 (Vec3, vec3)
import Math.Matrix4 (..)
import Math.RandomVector (..)

import Engine (..)

import Debug(log)

type TimeLeft a = { a | timeLeft : Float }

type Drop a = Massive (Spherical (Moving a))

-- newDrop : Vec3 -> Vec3 -> a -> Drop a
newDrop pos vel thing = orientDrop
    { thing | position <- pos, velocity <- vel, radius <- 1.0, mass <- 1.0 }
-- { positionupos vel (vec3 0 0 0) thing 1.0 1.0 }

orientDrop : Drop a -> Drop a
orientDrop d =
    let v = V3.toRecord d.velocity
    in { d | orientation <- V3.normalize (vec3 v.x 0 v.z) }

stepDrop : Time -> Drop a -> Drop a
stepDrop dt b = { b | position <- b.position `V3.add` (V3.scale (dt / second) b.velocity) }

-- timeStep : TimeLeft (Moving a) -> Moving a
timeStep : TimeLeft (Drop a) -> TimeLeft (Drop a)
timeStep x = { x  | position = x.position `V3.add` (V3.scale (x.timeLeft / second) x.velocity) }

stripTimeStep : TimeLeft a -> a
stripTimeStep x = { x - timeLeft }

setTimeLeft : Time -> a -> TimeLeft a
setTimeLeft dt x = { x | timeLeft = dt }

-- randomDrop : Signal a -> Signal (Drop a)
randomDrop thing =
    let pos = (V3.add (vec3 0 30 0)) <~ randomVec3 4.0
    in
        newDrop <~ pos ~ randomVec3 8.0 ~ thing

-- randomDrops : Int -> Signal [a] -> Signal [Drop a]
randomDrops n things =
    let poss = map (V3.add (vec3 0 30 0)) <~ randomVec3s n 4.0
    in
        zipWith3 newDrop <~ poss ~ randomVec3s n 8.0 ~ things

-- Run an update function over all combinations of 2 elements of an array
updatePairs : (a -> a -> Maybe (a, a)) -> Array.Array a -> Array.Array a
updatePairs f arr0 =
    let len = Array.length arr0
        -- update m n arr = log ("Updating ("++show m++","++show n++")") <| case f (Array.getOrFail m arr) (Array.getOrFail n arr) of
        update m n arr = case f (Array.getOrFail m arr) (Array.getOrFail n arr) of
            Nothing       -> arr
            Just (vm, vn) -> Array.set m vm . Array.set n vn <| arr
        row m arr = foldl (update m) arr [m+1..len-1]
    in foldl row arr0 [0..len-1]

{- Collision between two spheres
   http://www.gamasutra.com/view/feature/131424/pool_hall_lessons_fast_accurate_.php?page=2
-}
collide : Time -> TimeLeft (Massive (Spherical (Moving a))) -> TimeLeft (Massive (Spherical (Moving a)))
    -> Maybe (TimeLeft (Massive (Spherical (Moving a))), TimeLeft (Massive (Spherical (Moving a))))
collide dt a b =
    let square x = x*x

        sumRadii = a.radius + b.radius

        -- Vector C from center of A to center of B
        centerDisplacement = V3.sub b.position a.position
        centerDistance = V3.length centerDisplacement

        -- Distance between the closest surface points of the two spheres
        surfaceDistance = centerDistance - sumRadii

        -- Relative movement V in timestep dt
        relativeMovement = V3.scale (dt / second) (V3.sub a.velocity b.velocity)

        -- N: Normalized relative movement
        normRelMovement = V3.normalize relativeMovement

        -- Collision is only possible if the magnitude of the relative movement is
        -- larger than the distance between them
        tooFar = V3.length relativeMovement < surfaceDistance

        -- Are the spheres moving away from each other?
        movingAway = V3.dot centerDisplacement relativeMovement <= 0
    in
        case tooFar || movingAway of
        True -> Nothing
        False -> let

          -- Distance D to closest point to B on V
          distanceToPeriA = V3.dot {-N-}normRelMovement {-C-}centerDisplacement
          -- F == square of: the distance between A and B at the closest point on their
          -- current trajectories
          periSquared = square centerDistance - square distanceToPeriA

          -- Would the spheres collide at some point, if they maintained these velocities?
          passBy = {-F-}periSquared > square sumRadii
        in
          case passBy of
          True -> Nothing
          False -> let

            -- T == square of: distanceToPeriA - distanceToCollisionA
            ttt = square sumRadii - periSquared

            -- The distance A actually travels before collision, taking radii into account
            distanceToCollisionA = distanceToPeriA - sqrt ttt
  
            -- Have we even moved far enough to collide?
            notFarEnough = V3.length relativeMovement < distanceToCollisionA
          in
            case notFarEnough of
            True -> Nothing
            False -> let
  
              -- Relative movement vector of A to point of collision
              relativeCollision = V3.scale distanceToCollisionA normRelMovement

              -- Time taken before collision == collisionDelta * dt
              collisionDelta = V3.length relativeCollision / V3.length relativeMovement

              -- Movement vector of A to point of collision
              collisionVectorA = V3.scale (collisionDelta * dt / second) a.velocity

              -- Movement vector of B to point of collision
              collisionVectorB = V3.scale (collisionDelta * dt / second) b.velocity

              timeLeft = (1.0 - collisionDelta) * dt

              movedA = { a | position <- V3.add a.position collisionVectorA, timeLeft <- timeLeft }
              movedB = { b | position <- V3.add b.position collisionVectorB, timeLeft <- timeLeft }

              -- Projection of movement onto new centerDisplacement
              centerDisplacement' = V3.sub movedA.position movedB.position
              nnn = V3.normalize centerDisplacement'
              projA = V3.dot a.velocity nnn
              projB = V3.dot b.velocity nnn

              optimizedP = (2.0 * (projA - projB)) / (a.mass + b.mass)

              newVelA = V3.sub a.velocity (V3.scale (optimizedP * b.mass) nnn)
              newVelB = V3.add b.velocity (V3.scale (optimizedP * a.mass) nnn)

              -- XXX: Subsequent movement during this timestep should only move for
              -- the remaining time (1.0-collisionDelta)*dt

              collidedPair = ({movedA | velocity <- newVelA }, { movedB | velocity <- newVelB })

          in Just collidedPair

collisions : Time -> [Drop a] -> [TimeLeft (Drop a)]
collisions dt = Array.toList . updatePairs (collide dt) . Array.fromList . map (setTimeLeft dt)

bounds : Drop a -> Drop a
bounds b =
    let bound vs s low high = let dampVs = -vs * 0.99 in
            if | vs < 0 && s < low  -> dampVs
               | vs > 0 && s > high -> dampVs
               | otherwise          -> vs
        (x,y,z) = V3.toTuple b.position
        (vx,vy,vz) = V3.toTuple b.velocity
    in { b | velocity <- vec3 (bound vx x -20 20) (bound vy y (b.radius) 100) (bound vz z -20 20) }

gravity : a -> Vec3
gravity _ = vec3 0 -9.8 0

boundVelocity : Vec3 -> Vec3
boundVelocity v = let l = V3.length v in if (l<1) then (V3.scale (1/l) v) else v

moveDrops : Time -> [Drop a] -> [Drop a]
moveDrops dt boids =
    let
        nboids = length boids
        positions = map .position boids
        velocities = map .velocity boids
        sumPos = foldl1 V3.add positions
        sumVel = foldl1 V3.add velocities
        gs = map gravity boids
        applyRules b g = { b |
            velocity <- (b.velocity `V3.add` (V3.scale (dt / second) g)) }
        bs = zipWith applyRules boids gs
    in map (orientDrop . stripTimeStep . timeStep) . collisions dt . (map bounds) <| bs
