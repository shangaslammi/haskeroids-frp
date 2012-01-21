{-# LANGUAGE GADTs #-}
module Haskeroids.FRP.Draw
    ( Scene((:+:))
    , Drawable(..)
    , draw
    , render
    ) where

import Data.Monoid

import Haskeroids.FRP.Body
import Haskeroids.Geometry
import Haskeroids.Geometry.Transform
import Haskeroids.Render

class HasBody d => Drawable d where
    drawLines :: d -> [LineSegment]

data Scene where
    EmptyScene :: Scene
    DrawList   :: Drawable d => [d] -> Scene
    (:+:)      :: Scene -> Scene -> Scene

instance Monoid Scene where
    mempty  = EmptyScene
    mappend = (:+:)

draw :: Drawable d => [d] -> Scene
draw = DrawList

render :: Scene -> Float -> IO ()
render EmptyScene            _ = return ()
render (DrawList drawables)  t = mapM_ (renderDrawable t) drawables
render (sceneA :+: sceneB)   t = render sceneA t >> render sceneB t

renderDrawable :: Drawable d => Float -> d -> IO ()
renderDrawable t d = renderLines lns where
    lns = transform (body d) (drawLines d)

