module Haskeroids.Particles
    ( Particle(..)
    , NewParticle(..)
    , ParticleGen
    , addParticle
    , addParticles
    , runParticleGen
    , initParticle
    , interpolateParticle
    ) where

import Haskeroids.Render
import Haskeroids.Random
import Haskeroids.Geometry
import Haskeroids.Geometry.Body

import Control.Monad.Writer

mkParticleLine :: Float -> LineSegment
mkParticleLine sz = LineSegment ((0,sz),(0,-sz))

data Particle = Particle
    { particleBody :: Body
    , particleLife :: Int
    , particleLine :: LineSegment
    }

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
type ParticleGen    = Writer [NewParticle]

newtype ParticleSystem = ParticleSystem [Particle]

instance LineRenderable ParticleSystem where
    interpolatedLines f (ParticleSystem ps) = map (interpolateParticle f) ps

interpolateParticle :: Float -> Particle -> LineSegment
interpolateParticle f (Particle b _ ln) = transform b' ln where
    b' = interpolatedBody f b

initParticle :: NewParticle -> RandomParticle
initParticle (NewParticle p r d spr spd lt sz) = do
    e <- randomElliptical (0, r) (0, r)
    a <- randomBracket spr
    v <- randomBetween spd
    l <- randomBetween lt
    n <- randomAngle
    r <- randomBracket 10
    s <- randomBetween sz
    return Particle
        { particleBody = initBody (p /+/ e) n (polar v (a+d)) r
        , particleLife = l
        , particleLine = mkParticleLine s
        }

addParticle :: NewParticle -> ParticleGen ()
addParticle = tell . return

addParticles :: Int -> NewParticle -> ParticleGen ()
addParticles n = tell . replicate n

runParticleGen :: ParticleGen a -> (a, [NewParticle])
runParticleGen = runWriter
