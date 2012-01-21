{-# LANGUAGE Arrows #-}

module Haskeroids.FRP.Ship where

import Haskeroids.FRP.Body
import Haskeroids.FRP.Draw
import Haskeroids.FRP.Collisions
import Haskeroids.Geometry
import Haskeroids.Keyboard
import qualified Haskeroids.Controls as Ctrl

newtype Ship = Ship
    { shipBody :: Body
    }

instance HasBody Ship where
    body = shipBody

instance Drawable Ship where
    drawLines = shipLines

instance Collider Ship where
    collisionLines  = shipLines
    collisionRadius = shipSize

initBody = defaultBody
    { position = (400, 300)
    , friction = 0.96
    }

engineThrust = 0.7
turnRate     = 0.18

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
                | otherwise                   = []

playerShip :: Coroutine Keyboard (Ship, Event Bullet)
playerShip = proc kb -> do
    rec delayed <- delay initBody -< body
        forces  <- shipControls   -< (delayed, kb)
        body    <- shipBody       -< forces

    bullets <- shipGun -< kb

    returnA -< (Ship body, bullets)

    where
        shipBody = physicalBody initBody

shipGun :: Coroutine Keyboard (Event Bullet)
shipGun = undefined

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
