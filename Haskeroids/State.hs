module Haskeroids.State (
    GameState(..),
    initialGameState,
    ) where

import Haskeroids.Player
import Haskeroids.Geometry
import Haskeroids.Geometry.Body (initBody)
import Haskeroids.Render (LineRenderable(..))
import Haskeroids.Tick
import Haskeroids.Keyboard (Keyboard)

-- | Data type for tracking game state
data GameState = GameState { statePlayer :: Player }

instance LineRenderable GameState where
    interpolatedLines f = interpolatedLines f . statePlayer

instance Tickable GameState where
    tick = tickState
    
-- | Generate the initial game state
initialGameState :: GameState
initialGameState = GameState {
    statePlayer = initialPlayerState
    }

-- | Initial state for the player ship at center of the screen
initialPlayerState :: Player
initialPlayerState = Player $ initBody (400,300)

-- | Tick state into a new game state
tickState :: Keyboard -> GameState -> GameState
tickState kb (GameState pl) = GameState $ tick kb pl
