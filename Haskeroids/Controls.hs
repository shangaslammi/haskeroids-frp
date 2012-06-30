module Haskeroids.Controls
    ( keyTurnRight
    , keyTurnLeft
    , keyThruster
    , keyShoot
    ) where

import Graphics.UI.GLUT (Key(..), SpecialKey(..))

keyTurnRight = SpecialKey KeyRight
keyTurnLeft  = SpecialKey KeyLeft
keyThruster  = SpecialKey KeyUp
keyShoot     = Char ' '
