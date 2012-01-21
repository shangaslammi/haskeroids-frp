{-# LANGUAGE Arrows #-}
module Haskeroids.FRP.Bullet where

import Control.Arrow
import Control.Coroutine
import Control.Coroutine.FRP

import Haskeroids.FRP.Body
import Haskeroids.FRP.Draw
import Haskeroids.FRP.Collisions

newtype Bullet = Bullet { bulletBody :: Body }

bulletSpeed = 10.0
bulletLife  = 70

bullet :: Bullet -> Coroutine (Event Collision) (Maybe Bullet)
bullet (Bullet initBody) = proc collision -> do
    body <- constBody initBody  -< ()
    life <- scan (-) bulletLife -< 1

    let collided = not $ null collision

    returnA -< if life <= 0 || collided
        then Nothing
        else Just $ Bullet body
