{-# LANGUAGE Arrows #-}
module Haskeroids.Game where

import Control.Applicative
import Control.Arrow
import qualified Control.Category as C
import Control.Coroutine
import Control.Coroutine.FRP
import Control.Coroutine.FRP.Collections
import Control.Monad (replicateM)
import Data.List (foldl')

import qualified Haskeroids.Controls as Controls
import Haskeroids.Asteroid
import Haskeroids.Bullet
import Haskeroids.Collision
import Haskeroids.Geometry
import Haskeroids.Geometry.Body
import Haskeroids.Keyboard
import Haskeroids.Particles
import Haskeroids.Player
import Haskeroids.Random
import Haskeroids.Render

type RenderFunc    = Float -> IO ()
type GameCoroutine = Coroutine Keyboard RenderFunc

randomize :: RandomGen -> Coroutine (Random a) a
randomize gen = Coroutine $ \ra ->
    let (a, gen') = runRandom ra gen
    in (a, randomize gen')

game :: Coroutine Keyboard RenderFunc
game = proc kb -> do
    rec (pl, be) <- player (400,300) -< (kb, untag asteroids)
        newAsteroids <- mapC (randomize asRng)
                     <<< onceThen initialAsteroids C.id
                     <<< concatMapE spawnNewAsteroids
                     <<< delay []
                     -< breaks

        (bullets, hits) <- senders []
                        <<< delay [] *** (mapE bullet)
                        -< (asteroids, be)

        (asteroids, breaks) <- recvSenders []
                            <<< second (first (mapE asteroid))
                            -< ((),(newAsteroids, hits))

    death <- skipE 1 <<< edge -< playerAlive pl

    let pgen = sequence_ $ map collisionParticles (untag hits)
            ++ map asteroidExplosionParticles breaks
            ++ if not.null $ death
                    then [playerExplosionParticles pl]
                    else []

    newParticles    <- mapE particle
                    <<< randomize pgRng
                    <<< arr sequence
                    <<< mapE initParticle
                    -< snd (runParticleGen pgen)

    particles <- collection [] -< ((), newParticles)

    returnA -< \i -> do
        renderInterpolated i pl
        mapM_ (renderInterpolated i) bullets
        mapM_ (renderInterpolated i) $ untag asteroids
        renderLines $ map (interpolateParticle i) particles

    where
        initialAsteroids = replicate 3 genInitialAsteroid
        asRng = initRandomGen 123
        pgRng = initRandomGen 321

particle :: Particle -> Item () Particle
particle (Particle ibody ilife line) = proc () -> do
    pbody <- body ibody 1.0  -< []
    plife <- integrate ilife -< -1

    let p = Particle pbody plife line

    if plife > 0
        then returnA -< Just p
        else returnA -< Nothing

type AsteroidHit = Bullet
type AsteroidBreak = Asteroid

asteroid :: Asteroid
         -> RecvSend () AsteroidHit AsteroidBreak Asteroid
asteroid (Asteroid sz ibody hits lns) = proc ((), ev) -> do
    asbody <- body ibody 1.0 -< []
    ashits <- scanE (-) hits <<< constE 1 -< ev

    let asteroid = Asteroid sz asbody ashits lns

    if ashits > 0
        then returnA -< (Just asteroid, [])
        else returnA -< (Nothing, [asteroid])

bullet  :: Bullet
        -> Sender [Tagged Asteroid] (Tagged AsteroidHit) Bullet
bullet (Bullet ilife ibody) = proc asteroids -> do
    bbody <- body ibody 1.0  -< []
    blife <- integrate ilife -< -1

    let bul   = Bullet blife bbody
        colls = filter (collides bul . snd) asteroids
        hits  = map (\(rid,_) -> (rid, bul)) colls

    if null hits
        then if blife > 0
            then returnA -< (Just bul, [])
            else returnA -< (Nothing,  [])
        else returnA -< (Nothing, hits)

player :: Vec2 -> Coroutine (Keyboard, [Asteroid]) (Player, Event Bullet)
player ipos = proc (kb, asteroids) -> do
    thrust <- arr kbThrust -< kb
    turn   <- arr kbTurn   -< kb

    rec pbody   <- body initialBody shipDamping
                -< accEv ++ [SetRotation turn]

        accEv   <- mapE accelerate
                <<< watch ((>0).fst)
                -< (thrust, prevAngle pbody)

    rec let collision = any (collides prev) asteroids
        prev  <- delay initialPlayer -< pl
        alive <- scan (&&) True      -< not collision
        let pl = Player pbody alive

    rec bullets <- mapE fireBullet
                <<< watch shouldShoot
                -< (rof, pbody, kb)
        rof <- delay 0 <<< scan tickROF 0 -< bullets

    returnA -< (pl, bullets) where

        initialBody   = initBody ipos 0 (0,0) 0
        initialPlayer = Player initialBody True

        shouldShoot (rof, _, kb) = rof <= 0 && isKeyDown kb Controls.shoot
        fireBullet (_, body, _)  = initBullet (bodyPos body) (bodyAngle body)
        tickROF rof ev
            | null ev   = rof-1
            | otherwise = fireDelay

        kbThrust kb
            | isKeyDown kb Controls.thrust = 0.7
            | otherwise                    = 0.0

        kbTurn kb
            | isKeyDown kb Controls.turnLeft  = -0.18
            | isKeyDown kb Controls.turnRight =  0.18
            | otherwise                       =  0.00

        accelerate (thrust, angle) = Accelerate $ polar thrust angle

data BodyEvent = Accelerate Vec2 | SetRotation Float

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
