{-# LANGUAGE Arrows, NamedFieldPuns #-}
module Haskeroids.FRP.Body where

import Control.Arrow
import Control.Coroutine
import Control.Coroutine.FRP

import Haskeroids.Geometry
import Haskeroids.Geometry.Transform

data Body = Body
    { position   :: Position
    , velocity   :: Velocity
    , angle      :: Angle
    , angularVel :: AngularVelocity
    , friction   :: Friction
    }

type Acceleration    = Vec2
type AngularVelocity = Float
type Angle           = Float
type Friction        = Float

type BodyForces = (Acceleration, Event AngularVelocity)

class HasBody b where
    body :: b -> Body

defaultBody = Body
    { position   = (0,0)
    , velocity   = (0,0)
    , angle      = 0
    , angularVel = 0
    , friction   = 1.0
    }

interpolate :: Float -> Body -> Body
interpolate t body = body
    { position = position body /+/ velocity   body /* t
    , angle    = angle    body  +  angularVel body  * t
    }

physicalBody :: Body -> Coroutine BodyForces Body
physicalBody initial = proc (accel, setAngVel) -> do
    vel <- updateC (velocity initial) -< \v -> (v /+/ accel) /* fric
    pos <- updateC (position initial) -< \p -> wrapAround (p /+/ vel)
    ave <- stepE (angularVel initial) -< setAngVel
    ang <- integrate (angle initial)  -< ave

    (prevVel,prevAve) <- delay (velocity &&& angularVel $ initial) -< (vel, ave)

    returnA -< Body pos prevVel ang prevAve fric

    where
        fric = friction initial

        wrapAround (x,y) = (x',y') where
            x'
                | x < 0     = 800 + x
                | x > 800   = x - 800
                | otherwise = x
            y'
                | y < 0     = 600 + y
                | y > 600   = y - 600
                | otherwise = y

constBody :: Body -> Coroutine () Body
constBody initial = proc _ -> physicalBody initial -< ((0,0), [])

transform :: Body -> [LineSegment] -> [LineSegment]
transform b = map $ applyXform $ transformPt b

transformPt :: Body -> Vec2 -> Vec2
transformPt (Body {position, angle}) = translatePt position . rotatePt angle
