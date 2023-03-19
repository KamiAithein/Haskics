{-# LANGUAGE NamedFieldPuns #-}

module Draw where

import Prelude as P
import Image as I
import Geom as G
import Debug.Trace

import Control.Monad
import Control.Monad.ST

import Data.Array
import Data.Array.ST



fillWith :: I.RawPixel -> I.Image s -> ST s ()
fillWith with toFill@Image{imageImg} = do
    ((_, _), (width, height)) <- getBounds imageImg
    forM_ [(i, j) | i <- [1..height], j <- [1..width]] (\(i, j) -> writeArray imageImg (i, j) with)


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

fillRectWith :: Point -> Int -> Int -> I.RawPixel -> I.Image s -> ST s ()
fillRectWith (x,y) w h with img@Image{imageImg} = do
    ((_, _), (width, height)) <- getBounds imageImg 
    let iter =  [ (i, j) 
                | i <- [y..y+h]
                , j <- [x..x+w]
                , i <= height 
                    && j <= width
                    && i >= 1
                    && j >= 1]
    forM_ iter (\(i, j) -> do 
        prev <- readArray imageImg (i, j)
        writeArray imageImg (i, j) $ alphaBlend prev with)

antialias :: I.Image s -> Point -> (Point -> Int) -> I.RawPixel -> I.RawPixel
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

fillPick :: (Point -> Int) -> I.RawPixel -> I.RawPixel -> Point -> I.Image s -> I.RawPixel
fillPick aliaser' old new at img = 
    alphaBlend old $ antialias img at aliaser' new

fillCircleWith :: Point -> Int -> I.RawPixel -> I.Image s -> ST s ()
fillCircleWith (x,y) r with img@Image{imageImg} = do
    ((_, _), (width, height)) <- getBounds imageImg 
    let iter =  [(i, j) 
                | i <- [y-r..y+r]
                , j <- [x-r..x+r]
                -- , dist2 (i, j) (x, y) <= r^2 
                    , i <= height 
                    && j <= width
                    && i >= 1
                    && j >= 1]
    forM_ iter (\(i, j) -> do
        prev <- readArray imageImg (i, j)
        writeArray imageImg (i, j) $ fillPick aliaser' prev with (i, j) img)


    where   aliaser' :: Point -> Int
            aliaser' (i, j) =
                    let aa = antialiasAmt
                        pCenter'@(x', y') = (aa*x, aa*y)
                        r' = aa*r  
                        tester = (\p1 -> dist2 p1 pCenter' <= r'^2)
                    in aliaser (tester) (i, j)

fillLineWith :: Point -> Point -> Int -> I.RawPixel -> I.Image s -> ST s ()
fillLineWith (x1, y1) (x2, y2) strokeWidth with img@Image{imageImg}
    --diagonal decreasing (0, 10) (127, 100)
    | y1 /= y2 && x1 /= x2 = do
        ((_, _), (width, height)) <- getBounds imageImg
        let aa = antialiasAmt
        let (x1', y1') = (x1*aa, y1*aa)
        let (x2', y2') = (x2*aa, y2*aa)
        let dy' = y2'-y1'
        let dx' = x2'-x1'
        let m = (fromIntegral dy') / (fromIntegral dx') :: Float -- what if 0
        let b = y1' - dy'*x1' `div` dx'
        let aliaser' = (\(i, j) ->
                let aa = antialiasAmt
                    tester = (\(i', j') -> 
                        i' <= (P.round $ m * (fromIntegral $ j')) + b + strokeWidth
                        && i' >= (P.round $ m * (fromIntegral $ j')) + b - strokeWidth)
                in aliaser (tester) (i, j))
        let iter =  [ (i, j)
                    | i <- [P.min y1 y2..P.max y1 y2]
                    , j <- [P.min x1 x2..P.max x1 x2]
                    ,   i <= height 
                        && j <= width
                        && i >= 1
                        && j >= 1]
        forM_ iter (\(i, j) -> do
            prev <- readArray imageImg (i, j)
            writeArray imageImg (i, j) $ fillPick aliaser' prev with (i, j) img)
                    

    -- horizontal
    | y2 == y1 && x2 > x1 = do
        ((_, _), (width, height)) <- getBounds imageImg 
        let iter =  [(i, j)
                    | i <- [y2-strokeWidth `div` 2..y2+strokeWidth `div` 2]
                    , j <- [x1..x2]
                    , i <= height 
                        && j <= width
                        && i >= 1
                        && j >= 1]
        forM_ iter (\(i, j) -> do
            prev <- readArray imageImg (i, j)
            writeArray imageImg (i, j) $ alphaBlend prev with)
    | y2 == y1 && x2 < x1 = 
        fillLineWith (x2, y2) (x1, y1) strokeWidth with img

    -- vertical
    | x2 == x1 && y2 > y1 = do
        ((_, _), (width, height)) <- getBounds imageImg 
        let iter =  [(i, j)
                    | i <- [y1..y2]
                    , j <- [x2-strokeWidth `div` 2..x2+strokeWidth `div` 2]
                    , i <= height 
                        && j <= width
                        && i >= 1
                        && j >= 1]
        forM_ iter (\(i, j) -> do
            prev <- readArray imageImg (i, j)
            writeArray imageImg (i, j) $ alphaBlend prev with)
    | x2 == x1 && y2 < y1 = 
        fillLineWith (x2, y2) (x1, y1) strokeWidth with img

    | otherwise = undefined


-- https://web.archive.org/web/20141221050705/http://forum.devmaster.net/t/advanced-rasterization/6145
-- (y-y1) = m(x-x1)
-- y = m(x-x1) + y1
-- true + false -
halfspace :: Point -> Point -> Point -> Bool
halfspace (x1, y1) (x2, y2) (x, y) = 
    (x1 - x2) * (y1 - y) - (y2 - y1) * (x - x1) > 0

fillTriangleWith :: Point -> Point -> Point -> I.RawPixel -> I.Image s -> ST s ()
fillTriangleWith p1@(x1, y1) p2@(x2, y2) p3@(x3, y3) with img@Image{imageImg} = do
    ((_, _), (width, height)) <- getBounds imageImg
    let minx = minimum [x1, x2, x3]
    let miny = minimum [y1, y2, y3]
    let maxx = maximum [x1, x2, x3]
    let maxy = maximum [y1, y2, y3]

    let iter =  [(i, j)
                | i <- [miny..maxy]
                , j <- [minx..maxx]
                ,      halfspace p1 p2 (j, i)
                    && halfspace p2 p3 (j, i)
                    && halfspace p3 p1 (j, i)
                    && i <= height 
                    && j <= width
                    && i >= 1
                    && j >= 1]
    forM_ iter (\(i, j) -> do
        prev <- readArray imageImg (i, j)
        writeArray imageImg (i, j) $ alphaBlend (prev) with)


-- drawObjectOutlineWith :: G.Geom -> I.RawPixel -> I.RawImage -> I.RawImage
-- drawObjectOutlineWith G.Geom{vert=gVertices, face=gFaces} with canvas = 
--     foldl (\)