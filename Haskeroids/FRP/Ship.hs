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
import qualified Haskeroids.Controls as Ctrl

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

playerShip :: Coroutine (Keyboard, Event collision) (Maybe Ship, Event Bullet)
playerShip = switchWith playerDead $ proc kb -> do
    rec delayed <- delay initBody -< body
        forces  <- shipControls   -< (delayed, kb)
        body    <- shipBody       -< forces

    bullets <- shipGun -< (kb, body)

    returnA -< (Just (Ship body), bullets)

    where
        shipBody   = physicalBody initBody
        playerDead = const $ pure (Nothing, [])

shipControls :: Coroutine (Body, Keyboard) BodyForces
shipControls = proc (body, kb) -> do
    let ang = angle body
        (thrust, turn) = handleKeyboard kb

    returnA -< (polar thrust ang, turn)
    where
        handleKeyboard kb = (thrust, turn) where
            thrust
                | isKeyDown kb Ctrl.thrust = engineThrust
                | otherwise                = 0
            turn
                | isKeyDown kb Ctrl.turnLeft  = [-turnRate]
                | isKeyDown kb Ctrl.turnRight = [turnRate]
                | otherwise                   = [0]

shipGun :: Coroutine (Keyboard, Body) (Event Bullet)
shipGun = proc (kb, body) -> do
    rec let fireButton = isKeyDown kb Ctrl.shoot
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
