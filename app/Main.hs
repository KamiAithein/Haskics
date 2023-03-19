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

rectangleExample :: I.Image s -> ST s ()
rectangleExample img = do
    D.fillRectWith (0, 0) 50 50 (0, 255, 0, 255) img
    D.fillRectWith (50, 50) 50 50 (0, 0, 255, 200) img
    D.fillRectWith (0, 0) 100 100 (255, 0, 0, 100) img

-- fillRectWith :: Point -> Int -> Int -> I.RawPixel -> I.RawImage -> I.RawImage

linesExample :: I.Image s -> ST s ()
linesExample img = do

    D.fillWith (255, 0, 0, 0) img
    let strokeWidth = 1
    let commands = 
            [ ((0, 0),  (255, 255),  strokeWidth, (0, 0, 0, 255))
            , ((0, 255),  (255, 0),  strokeWidth, (0, 0, 0, 255))
            , ((0, 200),  (255, 55), strokeWidth, (0, 0, 0, 255))
            , ((0, 55),  (255, 200), strokeWidth, (0, 0, 0, 255))
            , ((55, 0),  (200, 255), strokeWidth, (0, 0, 0, 255))
            , ((200, 0),  (55, 255), strokeWidth, (0, 0, 0, 255))
            , ((0, 200), (255, 200), strokeWidth, (0, 0, 0, 255))
            ]
    forM_ commands (\(p1, p2, width, color) -> D.fillLineWith p1 p2 width color img)

triangleExample :: I.Image s -> ST s ()
triangleExample img = do 
    D.fillTriangleWith (100, 0) (100, 100) (0, 100) (0, 255, 0, 255) img

triangleExample2 :: I.Image s -> ST s ()
triangleExample2 img = do
    D.fillTriangleWith (100, 0) (100, 100) (0, 100) (0, 255, 0, 255) img
    D.fillTriangleWith (100, 0) (255, 255) (0, 100) (255, 0, 255, 125) img

circleExample :: I.Image s -> ST s ()
circleExample img = 
    D.fillCircleWith (120, 120) 50 (255, 0, 0, 255) img

run :: ST s (IO ())
run = do
    let width = 255
    let height = 255
    image <- I.createImage width height
    fillExample image
    linesExample image
    triangleExample image
    triangleExample2 image
    rectangleExample image
    circleExample image
    saveImage "assets/test.ppm" image

main :: IO ()
main = runST $ run
