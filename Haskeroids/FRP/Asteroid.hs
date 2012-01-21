{-# LANGUAGE Arrows #-}

module Haskeroids.FRP.Asteroid where

import Control.Arrow
import Control.Coroutine
import Control.Coroutine.FRP

import Haskeroids.Geometry
import Haskeroids.FRP.Body
import Haskeroids.FRP.Draw
import Haskeroids.FRP.Collisions


data Asteroid = Asteroid
    { asteroidBody  :: Body
    , asteroidSize  :: Size
    , asteroidLines :: [LineSegment]
    }

data Size = Small|Medium|Large deriving (Ord, Eq, Enum)

instance HasBody Asteroid where
    body = asteroidBody

instance Drawable Asteroid where
    drawLines = asteroidLines

instance Collider Asteroid where
    collisionLines  = asteroidLines
    collisionRadius = radius . asteroidSize

-- | Radius for an asteroid
radius :: Size -> Float
radius Small  = 14
radius Medium = 32
radius Large  = 70

-- | Hitpoints for an asteroid
maxHits :: Size -> Int
maxHits Small  = 3
maxHits Medium = 6
maxHits Large  = 9

-- | Number of vertices for an asteroid
numVertices :: Size -> Int
numVertices Small  = 9
numVertices Medium = 13
numVertices Large  = 17

asteroid :: Asteroid -> Coroutine ((), Event collision) (Maybe Asteroid)
asteroid initial = proc (_, collision) -> do
    body      <- constBody initBody -< ()
    hitpoints <- scanE (-) (maxHits size) <<< constE 1 -< collision

    returnA -< if hitpoints > 0
        then Just $ Asteroid body size lines
        else Nothing

    where
        initBody = asteroidBody initial
        size     = asteroidSize initial
        lines    = asteroidLines initial
