{-# LANGUAGE Arrows #-}
module Haskeroids.Game where

import Control.Applicative
import Control.Arrow
import Control.Coroutine
import Control.Coroutine.FRP
import Data.List (foldl')

import qualified Haskeroids.Controls as Controls
import Haskeroids.Asteroid
import Haskeroids.Collision
import Haskeroids.Geometry
import Haskeroids.Geometry.Body
import Haskeroids.Keyboard
import Haskeroids.Player
import Haskeroids.Render

type RenderFunc    = Float -> [LineSegment]
type GameCoroutine = Coroutine Keyboard RenderFunc


game :: Coroutine Keyboard RenderFunc
game = proc kb -> do
    pl <- player (400,300) -< (kb, [])

    returnA -< flip interpolatedLines pl

data BodyEvent = Accelerate Vec2 | SetRotation Float

player :: Vec2 -> Coroutine (Keyboard, [Asteroid]) Player
player ipos = proc (kb, asteroids) -> do
    thrust <- arr kbThrust -< kb
    turn   <- arr kbTurn   -< kb

    rec pbody   <- body initialBody 0.96
                -< accEv ++ [SetRotation turn]

        accEv   <- mapE accelerate
                <<< watch ((>0).fst)
                -< (thrust, prevAngle pbody)

    rec let collision = any (collides prev) asteroids
        prev  <- delay initialPlayer -< pl
        alive <- scan (&&) True      -< not collision
        let pl = Player pbody alive Nothing 0

    returnA -< pl where

        initialBody   = initBody ipos 0 (0,0) 0
        initialPlayer = Player initialBody True Nothing 0

        kbThrust kb
            | isKeyDown kb Controls.thrust = 0.7
            | otherwise                    = 0.0

        kbTurn kb
            | isKeyDown kb Controls.turnLeft  = -0.18
            | isKeyDown kb Controls.turnRight =  0.18
            | otherwise                       =  0.00

        accelerate (thrust, angle) = Accelerate $ polar thrust angle

body :: Body -> Float -> Coroutine (Event BodyEvent) Body
body (Body ipos iangle ivel irot _ _) fric = proc ev -> do
    (vel, rot)  <- scan processEvents (ivel, irot) -< ev

    angle       <- integrate iangle         -< rot
    (pos, prev) <- scan tickPos (ipos,ipos) -< vel
    prevAngle   <- delay iangle             -< angle

    returnA -< Body pos angle vel rot prev prevAngle where

        processEvents (v,r) ev = (v' /* fric, r') where
            (v', r') = foldl' step (v, r) ev

            step (v,r) ev = case ev of
                Accelerate a  -> (v /+/ a, r)
                SetRotation r -> (v, r)

        tickPos (pos,_) vel = (newPos, prevPos) where
            prevPos = pos  /+/ wrap
            newPos  = pos' /+/ wrap
            pos'    = pos /+/ vel
            wrap    = wrapper pos'
