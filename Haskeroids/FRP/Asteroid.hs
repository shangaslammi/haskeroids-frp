{-# LANGUAGE Arrows #-}

module Haskeroids.FRP.Asteroid where

import Control.Arrow
import Control.Coroutine
import Control.Coroutine.FRP
import Control.Coroutine.FRP.Collections

import Data.List (sort)

import Haskeroids.Geometry
import Haskeroids.Random
import Haskeroids.FRP.Body
import Haskeroids.FRP.Draw
import Haskeroids.FRP.Collisions


data Asteroid = Asteroid
    { asteroidBody  :: Body
    , asteroidSize  :: Size
    , asteroidLines :: [LineSegment]
    }

type RandomAsteroid = Random Asteroid
data Size = Small|Medium|Large deriving (Ord, Eq, Enum)
newtype Break = Break Asteroid

instance HasBody Asteroid where
    body = asteroidBody

instance Drawable Asteroid where
    drawLines = asteroidLines

instance Collider Asteroid where
    collisionLines  = asteroidLines
    collisionRadius = radius . asteroidSize

asteroid :: Asteroid -> Coroutine ((), Event collision) (Maybe Asteroid, Event Break)
asteroid initial = proc (_, collision) -> do
    body      <- constBody initBody -< ()
    hitpoints <- scanE (-) (maxHits size) <<< constE 1 -< collision

    let a = Asteroid body size lines
    returnA -< if hitpoints > 0
        then (Just a,  [])
        else (Nothing, [Break a])

    where
        initBody = asteroidBody initial
        size     = asteroidSize initial
        lines    = asteroidLines initial

asteroids :: Coroutine (TEvent collision) ([Tagged Asteroid], Event Break)
asteroids = proc collisions -> do
    rec (objs, breaks) <- recvSenders [] -< ((), (newAsteroids, collisions))
        newAsteroids <- mapE asteroid <<< rand
            <<< onceThen initial (concatMapE spawnNewAsteroids
                <<< delay []) -< breaks

    returnA -< (objs, breaks)
    where
        rand    = mapC $ randomize (initRandomGen 123)
        initial = replicate 3 genInitialAsteroid

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

-- | Spawn random asteroids
spawnNewAsteroids :: Break -> [RandomAsteroid]
spawnNewAsteroids (Break (Asteroid b sz _ ))
    | sz == Small = []
    | otherwise   = replicate 3 $ randomAsteroid (pred sz) (position b)

-- | Initialize a new asteroid with the given position, velocity and rotation
newAsteroid :: Size -> Vec2 -> Vec2 -> Float -> [LineSegment] -> Asteroid
newAsteroid sz pos v r = Asteroid body sz where
    body = defaultBody
        { position   = pos
        , angle      = 0
        , velocity   = v
        , angularVel = r
        }

randomAsteroid :: Size -> Vec2 -> RandomAsteroid
randomAsteroid sz pos = do
    let r = radius sz

    (dx,dy) <- randomPair r
    (vx,vy) <- randomPair (40.0/r)
    rot     <- randomBracket (3.0/r)
    lns     <- genAsteroidLines sz

    return $ newAsteroid sz (pos /+/ (dx,dy)) (vx,vy) rot lns

-- | Generate the line segments for an asteroid size
genAsteroidLines :: Size -> Random [LineSegment]
genAsteroidLines sz = do
    let numPts = numVertices sz
        r      = radius sz

    radii  <- nrandomR numPts (r*0.5, r)
    angvar <- nrandomR numPts (-0.01*pi, 0.01*pi)

    let step   = 2.0*pi/(fromIntegral numPts + 1)
        angles = sort $ zipWith (+) angvar [0.0,step..2.0*pi]
        points = zipWith polar radii angles

    return $ pointsToSegments $ points ++ [head points]

-- | Generate an initial asteroid in the level
genInitialAsteroid :: RandomAsteroid
genInitialAsteroid = randomElliptical xb yb >>= randomAsteroid Large where
    xb = (140, 400)
    yb = (140, 300)

