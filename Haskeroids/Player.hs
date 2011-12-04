module Haskeroids.Player
    ( Player(..)
    , playerExplosionParticles
    , engineParticles
    , fireDelay
    , shipDamping
    ) where

import Haskeroids.Geometry
import Haskeroids.Geometry.Body
import Haskeroids.Render (LineRenderable(..))
import Haskeroids.Keyboard (Keyboard, isKeyDown)
import Haskeroids.Controls
import Haskeroids.Asteroid
import Haskeroids.Collision
import Haskeroids.Bullet
import Haskeroids.Particles

import Data.Maybe (isJust)
import Control.Arrow ((***))
import Control.Monad (when)

-- | Data type for tracking current player state
data Player = Player
    { playerBody   :: Body
    , playerAlive  :: Bool
    }

-- | Constant for the ship size
shipSize :: Float
shipSize = 12.0

-- | Constant for the delay (in ticks) between firing bullets
fireDelay :: Int
fireDelay = 5

-- | Damping factor for ship velocity
shipDamping :: Float
shipDamping = 0.96

instance LineRenderable Player where
    interpolatedLines f (Player body alive)
        | not alive = []
        | otherwise = map (transform b') shipLines where
            b' = interpolatedBody f body

instance Collider Player where
    collisionCenter = bodyPos . playerBody
    collisionRadius = const shipSize
    collisionLines  = interpolatedLines 0

engineParticles :: Player -> ParticleGen ()
engineParticles (Player body _) = addParticles 2 $ NewParticle
    { npPosition  = bodyPos body /+/ polar (shipSize/3.0) emitDir
    , npRadius    = 0
    , npDirection = emitDir
    , npSpread    = pi/6.0
    , npSpeed     = (1.0, 4.0)
    , npLifeTime  = (5, 15)
    , npSize      = (1,1)
    } where emitDir   = bodyAngle body + pi

playerExplosionParticles :: Player -> ParticleGen ()
playerExplosionParticles p = addParticles 40 NewParticle
    { npPosition  = bodyPos b
    , npRadius    = shipSize / 2.0
    , npDirection = 0
    , npSpread    = 2*pi
    , npSpeed     = (2.0, 8.0)
    , npLifeTime  = (15, 50)
    , npSize      = (2,4)
    } where
        b = playerBody p

-- | List of lines that make up the ship hull
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
