{-# LANGUAGE Arrows #-}
module Haskeroids.FRP.Game where

import Control.Arrow
import Control.Coroutine
import Control.Coroutine.FRP
import Control.Coroutine.FRP.Collections
import Data.Maybe

import Haskeroids.Geometry
import Haskeroids.Keyboard
import Haskeroids.FRP.Asteroid
import Haskeroids.FRP.Body
import Haskeroids.FRP.Bullet
import Haskeroids.FRP.Collisions
import Haskeroids.FRP.Draw
import Haskeroids.FRP.Particles
import Haskeroids.FRP.Ship

game :: Coroutine Keyboard Scene
game = proc kb -> do
    rec
        (ship, newBlts, thrust) <- playerShip
            <<< second (delay [])
            -< (kb, plCollisions)

        blts <- bullets
            <<< second (delay [])
            -< (newBlts, bltCollisions)

        (asts, breaks) <- asteroids
            <<< delay []
            -< astCollisions

        let plCollisions = case ship of
                Nothing   -> []
                Just ship -> filter (collides ship) $ untag asts
            ~(astCollisions, bltCollisions) = unzip $ collisions asts blts

    dead      <- edge -< isJust ship
    shipDeath <- tagE <<< first (arr fromJust <<< delay Nothing) -< (ship, dead)

    let bulletParticles   = map bulletHitParticles bltCollisions
        asteroidParticles = map asteroidBreakParticles breaks
        deathParticles    = map shipDeathParticles shipDeath
        thrustParticles   = map engineParticles thrust

    ptcls <- particles -< concat
        $  bulletParticles
        ++ thrustParticles
        ++ asteroidParticles
        ++ deathParticles

    returnA -< draw (maybeToList ship)
        :+: draw (untag blts)
        :+: draw (untag asts)
        :+: draw ptcls


bulletHitParticles :: Tagged Bullet -> [NewParticle]
bulletHitParticles (_, b) = replicate 5 NewParticle
    { npPosition  = position body
    , npRadius    = 3
    , npDirection = emitDir
    , npSpread    = pi/2
    , npSpeed     = (2.0, 5.0)
    , npLifeTime  = (5, 15)
    , npSize      = (1,2)
    } where
        body    = bulletBody b
        emitDir = angle body + pi

asteroidBreakParticles :: Break -> [NewParticle]
asteroidBreakParticles (Break (Asteroid b sz _)) = replicate n NewParticle
    { npPosition  = position b
    , npRadius    = radius sz / 2.0
    , npDirection = 0
    , npSpread    = 2*pi
    , npSpeed     = (3.0, 6.0)
    , npLifeTime  = (15, 40)
    , npSize      = (1,3)
    } where
        n = round $ radius sz

shipDeathParticles :: Ship -> [NewParticle]
shipDeathParticles ship = replicate 40 NewParticle
    { npPosition  = position $ shipBody ship
    , npRadius    = shipSize / 2.0
    , npDirection = 0
    , npSpread    = 2*pi
    , npSpeed     = (2.0, 8.0)
    , npLifeTime  = (15, 50)
    , npSize      = (2,4)
    }

engineParticles :: Body -> [NewParticle]
engineParticles body = replicate 2 $ NewParticle
    { npPosition  = position body /+/ polar (shipSize/3.0) emitDir
    , npRadius    = 0
    , npDirection = emitDir
    , npSpread    = pi/6.0
    , npSpeed     = (1.0, 4.0)
    , npLifeTime  = (5, 15)
    , npSize      = (1,1)
    } where emitDir   = angle body + pi
