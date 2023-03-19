module Draw where

import Prelude as P
import Image as I
import Geom as G
import Debug.Trace

import Data.Array.IArray
import Data.Array.ST
import Control.Monad
import Control.Monad.ST

{-
I want it to look like this:

runSTCanvas $ do
    a
    b


I NEED TO REPLACE IMAGE ARRAY WITH IMAGE MARRAY

runSTCanvas $ a >> b
-}


fillWith :: I.RawPixel -> I.Image s -> I.Image s
fillWith with toFill = do 
    (MImage toFill') <- toFill
    mapArray (\_ -> with) toFill'
    return $ return toFill' 




type Point = (Int, Int)

antialiasAmt :: Int
antialiasAmt = 6

dist2 :: Point -> Point -> Int
dist2 (x1, y1) (x2, y2) = (x2-x1)^2 + (y2-y1)^2

lerp' :: Int -> Int -> Int -> Int
lerp' n1 n2 r255 = P.round $ lerp (fromIntegral n1) (fromIntegral n2) ((fromIntegral r255) / (fromIntegral 255))

lerp :: Float -> Float -> Float -> Float
lerp n1 n2 r = n1 + (n2 - n1)*r

-- let's just blend the second color on top of the first color...
alphaBlend :: I.RawPixel -> I.RawPixel -> I.RawPixel
alphaBlend (r1, g1, b1, a1) (r2, g2, b2, a2) = 
    ( lerp' r1 r2 a2
    , lerp' g1 g2 a2 
    , lerp' b1 b2 a2
    , P.min 255 (a1 + a2)
    )

-- fillRectWith :: Point -> Int -> Int -> I.RawPixel -> I.Image s -> I.MImage ()
-- fillRectWith (x,y) w h with (img) = do
--     (MImage img) <- img
--             mImg <- mImg'
--             let ((_, _), (width, height)) = bounds iImg
--             let iter =  [((i, j), alphaBlend (iImg ! (i, j)) with) 
--                         | i <- [y..y+h]
--                         , j <- [x..x+w]
--                         , i <= height 
--                             && j <= width
--                             && i >= 1
--                             && j >= 1]
--             forM_ iter (\((i, j), pixel) -> writeArray mImg (i, j) pixel)
    
            
-- fillRectWith (x,y) w h with cnv = 
--     let arr = runSTUArray $ do 
--             img <- cnv
--             let ((_, _), (width, height)) = bounds img 
            -- let iter =  [((i, j), alphaBlend (img ! (i, j)) with) 
            --             | i <- [y..y+h]
            --             , j <- [x..x+w]
            --             , i <= height 
            --                 && j <= width
            --                 && i >= 1
            --                 && j >= 1]
            
--             forM_ iter (\((i, j), pixel) -> writeArray (i, j) pixel cnv)
--     in (I.MImage (arr :: Array I.RawBounds I.RawPixel))
            
    

antialias :: I.RawImage -> Point -> (Point -> Int) -> I.RawPixel -> I.RawPixel
antialias img p aliaser (r, g, b, a) = 
    let a' = aliaser p
    in (r, g, b, a')

aliaser :: (Point -> Bool) -> Point -> Int
aliaser tester (i, j) = 
    let aa = antialiasAmt
        (i', j') = (aa*i, aa*j)
        tests = 
            [ (i' + di, j' + dj) 
            | di <- [0..(aa - 1)]
            , dj <- [0..(aa - 1)] ]
        nPass = length $ filter tester tests
        nTotal = length tests
    in P.round $ (fromIntegral nPass :: Float) / (fromIntegral nTotal :: Float) * 255.0 

fillPick :: (Point -> Int) -> I.RawPixel -> I.RawPixel -> Point -> I.RawImage -> I.RawPixel
fillPick aliaser' old new at img = 
    alphaBlend old $ antialias img at aliaser' new

-- fillCircleWith :: Point -> Int -> I.RawPixel -> I.Image -> I.Image
-- fillCircleWith (x,y) r with (I.MImage img) = 
--     let ((_, _), (width, height)) = bounds img 
--     in I.MImage $ img//
--         [((i, j), fillPick aliaser' (img ! (i, j)) with (i, j) img) 
--         | i <- [y-r..y+r]
--         , j <- [x-r..x+r]
--         -- , dist2 (i, j) (x, y) <= r^2 
--             , i <= height 
--             && j <= width
--             && i >= 1
--             && j >= 1]


--     where   aliaser' :: Point -> Int
--             aliaser' (i, j) =
--                     let aa = antialiasAmt
--                         pCenter'@(x', y') = (aa*x, aa*y)
--                         r' = aa*r  
--                         tester = (\p1 -> dist2 p1 pCenter' <= r'^2)
--                     in aliaser (tester) (i, j)

-- fillLineWith :: Point -> Point -> Int -> I.RawPixel -> I.Image -> I.Image
-- fillLineWith (x1, y1) (x2, y2) strokeWidth with (I.MImage img)
--     --diagonal decreasing (0, 10) (127, 100)
--     | y1 /= y2 && x1 /= x2 = 
--         let ((_, _), (width, height)) = bounds img 
--             aa = antialiasAmt
--             (x1', y1') = (x1*aa, y1*aa)
--             (x2', y2') = (x2*aa, y2*aa)
--             dy' = y2'-y1'
--             dx' = x2'-x1'
--             m = (fromIntegral dy') / (fromIntegral dx') :: Float -- what if 0
--             b = y1' - dy'*x1' `div` dx'
--             aliaser' = (\(i, j) ->
--                 let aa = antialiasAmt
--                     tester = (\(i', j') -> 
--                         i' <= (P.round $ m * (fromIntegral $ j')) + b + strokeWidth
--                         && i' >= (P.round $ m * (fromIntegral $ j')) + b - strokeWidth)
--                 in aliaser (tester) (i, j))
--         in I.MImage $ img//
--             [((i, j), fillPick aliaser' (img ! (i, j)) with (i, j) img)
--             | i <- [P.min y1 y2..P.max y1 y2]
--             , j <- [P.min x1 x2..P.max x1 x2]
--             ,   i <= height 
--                 && j <= width
--                 && i >= 1
--                 && j >= 1]
                    

--     -- horizontal
--     | y2 == y1 && x2 > x1 = 
--         let ((_, _), (width, height)) = bounds img 
--         in I.MImage $ img//
--             [((i, j), alphaBlend (img ! (i, j)) with)
--             | i <- [y2-strokeWidth `div` 2..y2+strokeWidth `div` 2]
--             , j <- [x1..x2]
--             , i <= height 
--                 && j <= width
--                 && i >= 1
--                 && j >= 1]
--     | y2 == y1 && x2 < x1 = 
--         fillLineWith (x2, y2) (x1, y1) strokeWidth with (I.MImage img)
--     -- vertical
--     | x2 == x1 && y2 > y1 = 
--         let ((_, _), (width, height)) = bounds img 
--         in I.MImage $ img//
--             [((i, j), alphaBlend (img ! (i, j)) with)
--             | i <- [y1..y2]
--             , j <- [x2-strokeWidth `div` 2..x2+strokeWidth `div` 2]
--             , i <= height 
--                 && j <= width
--                 && i >= 1
--                 && j >= 1]
--     | x2 == x1 && y2 < y1 = 
--         fillLineWith (x2, y2) (x1, y1) strokeWidth with (I.MImage img)
--     | otherwise = undefined


-- -- https://web.archive.org/web/20141221050705/http://forum.devmaster.net/t/advanced-rasterization/6145
-- -- (y-y1) = m(x-x1)
-- -- y = m(x-x1) + y1
-- -- true + false -
-- halfspace :: Point -> Point -> Point -> Bool
-- halfspace (x1, y1) (x2, y2) (x, y) = 
--     (x1 - x2) * (y1 - y) - (y2 - y1) * (x - x1) > 0

-- fillTriangleWith :: Point -> Point -> Point -> I.RawPixel -> I.Image -> I.Image
-- fillTriangleWith p1@(x1, y1) p2@(x2, y2) p3@(x3, y3) with (I.MImage img) =
--     let ((_, _), (width, height)) = bounds img
--         minx = minimum [x1, x2, x3]
--         miny = minimum [y1, y2, y3]
--         maxx = maximum [x1, x2, x3]
--         maxy = maximum [y1, y2, y3]

--     in I.MImage $ img//
--         [((i, j), alphaBlend (img ! (i, j)) with)
--         | i <- [miny..maxy]
--         , j <- [minx..maxx]
--         ,      halfspace p1 p2 (j, i)
--             && halfspace p2 p3 (j, i)
--             && halfspace p3 p1 (j, i)
--             && i <= height 
--             && j <= width
--             && i >= 1
--             && j >= 1]


-- -- drawObjectOutlineWith :: G.Geom -> I.RawPixel -> I.RawImage -> I.RawImage
-- -- drawObjectOutlineWith G.Geom{vert=gVertices, face=gFaces} with canvas = 
-- --     foldl (\)