module Main (main) where

import Image as I
import Draw as D
import Data.Array.IArray
import Prelude as P

import Control.Monad


fillExample :: Image s -> Image s
fillExample canvas = do runST $
    D.fillWith (255, 0, 0, 120) canvas
    D.fillWith (0, 255, 255, 120) canvas

-- rectangleExample :: Image s -> Image s
-- rectangleExample canvas = do runST $
--     D.fillRectWith (0, 0) 50 50 (0, 255, 0, 255) canvas 
--     D.fillRectWith (50, 50) 50 50 (0, 0, 255, 200) canvas
--     D.fillRectWith (0, 0) 100 100 (255, 0, 0, 100) canvas
-- fillRectWith :: Point -> Int -> Int -> I.RawPixel -> I.RawImage -> I.RawImage

-- linesExample :: Image -> Image
-- linesExample canvas = do
--     D.fillWith (255, 0, 0, 0) canvas
    
--     let strokeWidth = 1
--     let commands = 
--             [ ((0, 0),  (255, 255),  strokeWidth, (0, 0, 0, 255))
--             , ((0, 255),  (255, 0),  strokeWidth, (0, 0, 0, 255))
--             , ((0, 200),  (255, 55), strokeWidth, (0, 0, 0, 255))
--             , ((0, 55),  (255, 200), strokeWidth, (0, 0, 0, 255))
--             , ((55, 0),  (200, 255), strokeWidth, (0, 0, 0, 255))
--             , ((200, 0),  (55, 255), strokeWidth, (0, 0, 0, 255))
--             , ((0, 200), (255, 200), strokeWidth, (0, 0, 0, 255))
--             ]
--     forM commands (\(p1, p2, width, color) -> D.fillLineWith p1 p2 width color canvas)
--     canvas

-- triangleExample :: Image -> Image
-- triangleExample canvas  = D.fillTriangleWith (100, 0) (100, 100) (0, 100) (0, 255, 0, 255) canvas

-- triangleExample2 :: Image -> Image
-- triangleExample2 canvas = do
--     D.fillTriangleWith (100, 0) (100, 100) (0, 100) (0, 255, 0, 255) canvas
--     D.fillTriangleWith (100, 0) (255, 255) (0, 100) (255, 0, 255, 125) canvas

-- circleExample :: Image -> Image
-- circleExample canvas = D.fillCircleWith (120, 120) 50 (255, 0, 0, 255) canvas

run :: IO ()
run = do
    let width = 255
    let height = 255
    let image = do 
            let image = I.createImage width height
            -- linesExample image
            -- triangleExample image
            -- triangleExample2 image
            -- rectangleExample image
            fillExample image
            -- circleExample image
    saveImage image "assets/test.ppm"
    putStrLn "pottato"

main :: IO ()
main = run
