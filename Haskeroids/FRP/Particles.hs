{-# LANGUAGE Arrows #-}

module Haskeroids.FRP.Particles (Particle, NewParticle(..), particles) where

import Control.Arrow
import Control.Coroutine
import Control.Coroutine.FRP
import Control.Coroutine.FRP.Collections

import Haskeroids.Geometry
import Haskeroids.Random
import Haskeroids.FRP.Draw
import Haskeroids.FRP.Body

data Particle = Particle
    { particleBody  :: Body
    , particleLife  :: Int
    , particleLines :: [LineSegment]
    }

instance HasBody Particle where
    body = particleBody

instance Drawable Particle where
    drawLines = particleLines

particle :: Particle -> Coroutine () (Maybe Particle)
particle initial = proc _ -> do
    body <- constBody (particleBody initial) -< ()
    life <- scan (-) (particleLife initial) -< 1

    returnA -< if life > 0
        then Just $ Particle body life lines
        else Nothing

    where
        lines = particleLines initial

particles :: Coroutine (Event NewParticle) [Particle]
particles = proc new -> do
    newParticles <- mapE particle <<< rand <<< mapE initParticle -< new
    collection [] -< ((), newParticles)
    where
        rand = mapC $ randomize (initRandomGen 1122)

mkParticleLine :: Float -> LineSegment
mkParticleLine sz = LineSegment ((0,sz),(0,-sz))

data NewParticle = NewParticle
    { npPosition  :: Vec2
    , npRadius    :: Float
    , npDirection :: Float
    , npSpread    :: Float
    , npSpeed     :: SpeedRange
    , npLifeTime  :: LifeRange
    , npSize      :: SizeRange
    }

type RandomParticle = Random Particle
type Direction      = Float
type Spread         = Float
type SpeedRange     = (Float, Float)
type LifeRange      = (Int, Int)
type SizeRange      = (Float, Float)

initParticle :: NewParticle -> RandomParticle
initParticle (NewParticle p r d spr spd lt sz) = do
    e <- randomElliptical (0, r) (0, r)
    a <- randomBracket spr
    v <- randomBetween spd
    l <- randomBetween lt
    n <- randomAngle
    r <- randomBracket 10
    s <- randomBetween sz
    let vel = polar v (a+d)
    return Particle
        { particleBody  = defaultBody
            { position = (p /+/ e)
            , velocity = vel
            , angle    = n
            , angularVel = r

            , prevVelocity = vel
            , prevAngularVel = r
            }
        , particleLife  = l
        , particleLines = [mkParticleLine s]
        }
