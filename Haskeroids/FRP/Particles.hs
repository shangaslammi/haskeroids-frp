
module Haskeroids.FRP.Particles where

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
