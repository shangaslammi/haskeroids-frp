{-# LANGUAGE Arrows #-}
module Haskeroids.FRP.Game where

import Control.Arrow
import Control.Coroutine
import Control.Coroutine.FRP
import Control.Coroutine.FRP.Collections
import Data.Maybe

import Haskeroids.Keyboard
import Haskeroids.FRP.Asteroid
import Haskeroids.FRP.Bullet
import Haskeroids.FRP.Collisions
import Haskeroids.FRP.Draw
import Haskeroids.FRP.Particles
import Haskeroids.FRP.Ship

game :: Coroutine Keyboard Scene
game = proc kb -> do
    rec (ship, newBlts) <- playerShip -< (kb, plCollisions)
        blts            <- bullets    -< (newBlts, bltCollisions)
        (asts, breaks)  <- asteroids  -< astCollisions

        let plCollisions = case ship of
                Nothing   -> []
                Just ship -> filter (collides ship) $ untag asts

            (astCollisions, bltCollisions) = unzip $ collisions asts blts

    returnA -< draw (maybeToList ship)
        :+: draw (untag blts)
        :+: draw (untag asts)
