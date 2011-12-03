{-# LANGUAGE Arrows #-}
module Haskeroids.Game where

import Control.Applicative
import Control.Arrow
import Control.Coroutine
import Control.Coroutine.FRP
import Control.Coroutine.FRP.Collections
import Data.List (foldl')

import qualified Haskeroids.Controls as Controls
import Haskeroids.Asteroid
import Haskeroids.Bullet
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
    (pl, be) <- player (400,300) -< (kb, [])

    bullets <- collection [] <<< second (mapE bullet) -< ((),be)

    returnA -< \i ->
        interpolatedLines i pl ++ concatMap (interpolatedLines i) bullets

data BodyEvent = Accelerate Vec2 | SetRotation Float

bullet :: Bullet -> Coroutine () (Maybe Bullet)
bullet (Bullet ilife ibody) = proc () -> do
    bbody <- body ibody 1.0  -< []
    blife <- integrate ilife -< -1

    if blife > 0
        then returnA -< (Just $ Bullet blife bbody)
        else returnA -< Nothing

player :: Vec2 -> Coroutine (Keyboard, [Asteroid]) (Player, Event Bullet)
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

    rec bullets <- mapE fireBullet
                <<< watch shouldShoot
                -< (rof, pbody, kb)
        rof <- delay 0 <<< scan tickROF 0 -< bullets

    returnA -< (pl, bullets) where

        initialBody   = initBody ipos 0 (0,0) 0
        initialPlayer = Player initialBody True Nothing 0

        shouldShoot (rof, _, kb) = rof <= 0 && isKeyDown kb Controls.shoot
        fireBullet (_, body, _)  = initBullet (bodyPos body) (bodyAngle body)
        tickROF rof ev
            | null ev   = rof-1
            | otherwise = 5

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
