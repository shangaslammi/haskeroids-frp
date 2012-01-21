{-# LANGUAGE Arrows #-}
module Haskeroids.FRP.Bullet where

import Control.Arrow
import Control.Coroutine
import Control.Coroutine.FRP

import Haskeroids.Geometry
import Haskeroids.FRP.Body
import Haskeroids.FRP.Draw
import Haskeroids.FRP.Collisions

newtype Bullet = Bullet { bulletBody :: Body }

bulletSpeed = 10.0
bulletLife  = 70

instance HasBody Bullet where
    body = bulletBody

instance Drawable Bullet where
    drawLines = const [LineSegment ((0,-bulletSpeed/2.0),(0,bulletSpeed/2.0))]

instance Collider Bullet where
    collisionRadius = const bulletSpeed
    collisionLines  = const [LineSegment ((0,-bulletSpeed/2.0),(0,bulletSpeed))]

bulletFrom :: Float -> Body -> Bullet
bulletFrom rng body = Bullet $ defaultBody
    { position = position body /+/ lead
    , velocity = vel
    , angle    = ang

    , prevVelocity = vel
    } where
        ang  = angle body
        vel  = polar bulletSpeed ang
        lead = polar ang rng

bullet :: Bullet -> Coroutine (Event Collision) (Maybe Bullet)
bullet (Bullet initBody) = proc collision -> do
    body <- constBody initBody  -< ()
    life <- scan (-) bulletLife -< 1

    let collided = not $ null collision

    returnA -< if life <= 0 || collided
        then Nothing
        else Just $ Bullet body
