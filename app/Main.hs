{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Image as I
import Draw as D
import Data.Array.IArray
import Prelude as P

import Control.Monad
import Control.Monad.ST

fillExample :: I.Image s -> ST s ()
fillExample img = do
    D.fillWith (255, 0, 0, 120) img
    D.fillWith (0, 255, 0, 120) img

-- rectangleExample :: Canvas -> Canvas
-- rectangleExample canvas = 
--     let canvas'  = D.fillRectWith (0, 0) 50 50 (0, 255, 0, 255) canvas
--         canvas'' = D.fillRectWith (50, 50) 50 50 (0, 0, 255, 200) canvas'
--     in D.fillRectWith (0, 0) 100 100 (255, 0, 0, 100) canvas''
-- -- fillRectWith :: Point -> Int -> Int -> I.RawPixel -> I.RawImage -> I.RawImage

-- linesExample :: Canvas -> Canvas
-- linesExample canvas = 

--     let canvas' = D.fillWith (255, 0, 0, 0) canvas
--         strokeWidth = 1
--         commands = 
--             [ ((0, 0),  (255, 255),  strokeWidth, (0, 0, 0, 255))
--             , ((0, 255),  (255, 0),  strokeWidth, (0, 0, 0, 255))
--             , ((0, 200),  (255, 55), strokeWidth, (0, 0, 0, 255))
--             , ((0, 55),  (255, 200), strokeWidth, (0, 0, 0, 255))
--             , ((55, 0),  (200, 255), strokeWidth, (0, 0, 0, 255))
--             , ((200, 0),  (55, 255), strokeWidth, (0, 0, 0, 255))
--             , ((0, 200), (255, 200), strokeWidth, (0, 0, 0, 255))
--             ]
--     in P.foldl (\acc (p1, p2, width, color) -> D.fillLineWith p1 p2 width color acc) canvas' commands

-- triangleExample :: Canvas -> Canvas
-- triangleExample canvas  = D.fillTriangleWith (100, 0) (100, 100) (0, 100) (0, 255, 0, 255) canvas

-- triangleExample2 :: Canvas -> Canvas
-- triangleExample2 canvas = 
--     let canvas' = D.fillTriangleWith (100, 0) (100, 100) (0, 100) (0, 255, 0, 255) canvas
--     in D.fillTriangleWith (100, 0) (255, 255) (0, 100) (255, 0, 255, 125) canvas'

-- circleExample :: Canvas -> Canvas
-- circleExample canvas = D.fillCircleWith (120, 120) 50 (255, 0, 0, 255) canvas

run :: ST s (IO ())
run = do
    let width = 255
    let height = 255
    image <- I.createImage width height
    fillExample image
    -- let image' = linesExample image
    -- let image' = triangleExample image
    -- let image' = triangleExample2 image
    -- let image' = rectangleExample image
    -- let image' = circleExample image
    saveImage "assets/test.ppm" image

main :: IO ()
main = runST $ run
