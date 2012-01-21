{-# LANGUAGE GADTs #-}
module Haskeroids.FRP.Draw (Scene, Drawable, draw, render) where

import Data.Monoid

import Haskeroids.FRP.Body
import Haskeroids.Geometry
import Haskeroids.Geometry.Transform
import Haskeroids.Render

class HasBody d => Drawable d where
    drawLines :: d -> [LineSegment]

data Scene where
    EmptyScene :: Scene
    SingleDraw :: Drawable d => d -> Scene
    (:+:)      :: Scene -> Scene -> Scene

instance Monoid Scene where
    mempty  = EmptyScene
    mappend = (:+:)

draw :: Drawable d => d -> Scene
draw = SingleDraw

render :: Scene -> Float -> IO ()
render EmptyScene            _ = return ()
render (SingleDraw drawable) t = renderDrawable drawable t
render (sceneA :+: sceneB)   t = render sceneA t >> render sceneB t

renderDrawable :: Drawable d => d -> Float -> IO ()
renderDrawable d t = renderLines lns where
    lns = transform (body d) (drawLines d)

