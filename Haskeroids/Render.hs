
module Haskeroids.Render (renderLines) where

import Graphics.Rendering.OpenGL
import Haskeroids.Geometry
import Haskeroids.Geometry.Transform

-- | Render a list of line segments using OpenGL
renderLines :: [LineSegment] -> IO ()
renderLines lns = do
    currentColor $= Color4 0.9 0.9 0.9 1.0
    renderPrimitive Lines $ mapM_ lineVertices $ wrapLines lns

-- | Generate extra lines for segments that go out of the screen
wrapLines :: [LineSegment] -> [LineSegment]
wrapLines = foldr go [] where
    go l@(LineSegment (p,p')) acc
        | both      = l': l'' : acc
        | first     = l : l'  : acc
        | second    = l : l'' : acc
        | otherwise = l : acc
        where
            both   = first && second && w/= w'
            first  = w  /= (0,0)
            second = w' /= (0,0)

            w   = wrapper p
            w'  = wrapper p'
            l'  = applyXform (translatePt w)  l
            l'' = applyXform (translatePt w') l

-- | Generate the OpenGL vertices of a line segment
lineVertices :: LineSegment -> IO ()
lineVertices (LineSegment (p,p')) = ptVertex p >> ptVertex p'

-- | Generate an OpenGL vertex from a point
ptVertex :: Vec2 -> IO ()
ptVertex = vertex . uncurry Vertex2 . convert where
    convert :: Vec2 -> (GLfloat, GLfloat)
    convert (x,y) = (realToFrac x, realToFrac y)
