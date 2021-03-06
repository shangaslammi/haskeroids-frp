{-# LANGUAGE Arrows #-}

module Haskeroids.FRP.Ship where

import Control.Arrow
import Control.Applicative
import Control.Coroutine
import Control.Coroutine.FRP

import Haskeroids.FRP.Body
import Haskeroids.FRP.Bullet
import Haskeroids.FRP.Draw
import Haskeroids.FRP.Collisions
import Haskeroids.Geometry
import Haskeroids.Keyboard
import Haskeroids.Controls

newtype Ship = Ship { shipBody :: Body }

instance HasBody Ship where
    body = shipBody

instance Drawable Ship where
    drawLines = const shipLines

instance Collider Ship where
    collisionLines  = const shipLines
    collisionRadius = const shipSize

initBody = defaultBody
    { position = (400, 300)
    , friction = 0.96
    }

engineThrust = 0.7
turnRate     = 0.18
fireRate     = 10

type Thrust = Body


playerShip :: Coroutine (Keyboard, Event collision) (Maybe Ship, Event Bullet, Event Thrust)
playerShip = switchWith (const playerDead) playerAlive where
    playerAlive = proc kb -> do
        let keyDown = isKeyDown kb
            thrust
                | keyDown keyThruster = engineThrust
                | otherwise           = 0
            turn
                | keyDown keyTurnLeft  = [-turnRate]
                | keyDown keyTurnRight = [turnRate]
                | otherwise            = [0]

        rec let acceleration = polar thrust direction
            body      <- shipBody               -< (acceleration, turn)
            direction <- delay (angle initBody) -< angle body

        bullets <- shipGun -< (kb, body)

        let thrustEvent = if thrust > 0 then [body] else []

        returnA -< (Just (Ship body), bullets, thrustEvent)


    shipBody   = physicalBody initBody
    playerDead = pure (Nothing, [], [])


shipGun :: Coroutine (Keyboard, Body) (Event Bullet)
shipGun = proc (kb, body) -> do
    rec let fireButton = isKeyDown kb keyShoot
            canFire    = recharge <= 0
            bulletEv
                | fireButton && canFire = [bulletFrom shipSize body]
                | otherwise             = []
        recharge <- delay 0 <<< rechargeCounter -< bulletEv

    returnA -< bulletEv
    where
        rechargeCounter = proc ev ->
            updateC 0 -< if null ev then (subtract 1) else (const fireRate)


shipSize :: Float
shipSize = 12.0

shipLines :: [LineSegment]
shipLines = pointsToSegments shipPoints

shipPoints :: [Vec2]
shipPoints = map (uncurry polar . multiply) points where
    multiply = (shipSize*) *** (pi*)
    points   =
       [ (1.0, 0.0)
       , (1.0, 0.7)
       , (0.2, 1.0)
       , (1.0, 1.3)
       , (1.0, 0.0)
       ]
