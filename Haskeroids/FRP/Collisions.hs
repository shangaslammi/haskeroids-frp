
module Haskeroids.FRP.Collisions where

import Control.Applicative
import Control.Coroutine.FRP.Collections
import Data.Maybe

import Haskeroids.Geometry
import Haskeroids.FRP.Body

class HasBody c => Collider c where
    collisionRadius :: c -> Float
    collisionLines  :: c -> [LineSegment]

data Collision = Collision

collisions :: (Collider a, Collider b) => [Tagged a] -> [Tagged b] -> (TEvent Collision, TEvent Collision)
collisions as bs = unzip $ catMaybes $ collision <$> as <*> bs where
    collision (atag, a) (btag, b)
        | collides a b = Just ((atag, Collision), (btag, Collision))
        | otherwise    = Nothing

collides :: (Collider a, Collider b) => a -> b -> Bool
collides c c' = canCollide && doesCollide where
    canCollide  = distSqr < radius*radius
    doesCollide = or $ lineCollision <$> cl <*> cl'

    distSqr = ptDistanceSqr center center'
    radius  = collisionRadius c + collisionRadius c'

    cl  = transform (body c) (collisionLines c)
    cl' = transform (body c') (collisionLines c')

    center  = position $ body c
    center' = position $ body c'

-- | Test if two line segments intersect
lineCollision :: LineSegment -> LineSegment -> Bool
lineCollision (LineSegment ((x1,y1),(x2,y2))) (LineSegment ((x3,y3),(x4,y4)))
    | d == 0    = False
    | otherwise = ua >= 0 && ua <= 1 && ub >= 0 && ub <= 1 where
        ua = na/d
        ub = nb/d
        d  = (y4-y3)*(x2-x1) - (x4-x3)*(y2-y1)
        na = (x4-x3)*(y1-y3) - (y4-y3)*(x1-x3)
        nb = (x2-x1)*(y1-y3) - (y2-y1)*(x1-x3)
