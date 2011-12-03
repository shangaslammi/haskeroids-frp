{-# LANGUAGE Arrows #-}
module Haskeroids.Game where

import Control.Applicative
import Control.Arrow
import Control.Coroutine
import Control.Coroutine.FRP

import Haskeroids.Keyboard
import Haskeroids.Geometry
import Haskeroids.Geometry.Body

type RenderFunc = Float -> [LineSegment]

game :: Coroutine Keyboard RenderFunc
game = undefined

data BodyEvent = Accelerate Vec2 | SetRotation Float

body :: Body -> Float -> Coroutine (Event BodyEvent) Body
body (Body ipos iangle ivel irot _ _) fric = proc ev -> do
    (vel, rot)  <- scanE processEvents (ivel, irot) -< ev

    angle       <- integrate iangle         -< rot
    (pos, prev) <- scan tickPos (ipos,ipos) -< vel
    prevAngle   <- delay iangle             -< angle

    returnA -< Body pos angle vel rot prev prevAngle where

        processEvents (v,r) ev = case ev of
            Accelerate a  -> (v /+/ a, r)
            SetRotation r -> (v, r)

        tickPos (pos,_) vel = (newPos, prevPos) where
            prevPos = pos  /+/ wrap
            newPos  = pos' /+/ wrap
            pos'    = (pos /+/ vel) /* fric
            wrap    = wrapper pos'
