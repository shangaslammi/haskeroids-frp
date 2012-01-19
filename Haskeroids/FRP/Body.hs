{-# LANGUAGE Arrows #-}
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

    , prevVelocity   :: Velocity
    , prevAngularVel :: AngularVelocity
    }

type Acceleration    = Vec2
type AngularVelocity = Float
type Angle           = Float
type Friction        = Float

interpolate :: Float -> Body -> Body
interpolate t body = body
    { position = position body /-/ prevVelocity   body /* t'
    , angle    = angle    body  -  prevAngularVel body  * t'
    }
    where t' = (t - 1.0)

physicalBody :: Body -> Coroutine (Acceleration, Event AngularVelocity) Body
physicalBody initial = proc (accel, setAngVel) -> do
    vel <- updateC (velocity initial) -< \v -> (v /+/ accel) /* fric
    pos <- updateC (position initial) -< \p -> wrapAround (p /+/ vel)
    ave <- stepE (angularVel initial) -< setAngVel
    ang <- integrate (angle initial)  -< ave

    (prevVel,prevAve) <- delay (velocity &&& angularVel $ initial) -< (vel, ave)

    returnA -< Body pos vel ang ave fric prevVel prevAve

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
