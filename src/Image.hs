module Image where

import Prelude as P
import System.IO
import Numeric
import Data.ByteString as BS

import Control.Monad.ST
import Data.Array.IArray

type RawPixel = (Int, Int, Int, Int)    -- r g b a
type RawBounds = (Int, Int)             -- y, x
type RawImage = Array RawBounds RawPixel


-- -- map each pixel, using f to map
map2 :: (a -> b) -> [[a]] ->  [[b]]
map2 f arr =  P.map (\row -> P.map f row) arr

createRawImage :: Int -> Int -> RawImage
createRawImage width height = array ((1,1), (width,height)) $ [((i, j), (0, 0, 0, 0)) | i <- [1..width], j <- [1..height]]

saveRawImage :: RawImage -> String -> IO ()
saveRawImage imgArr name =
    let ((_, _), (width, height)) = bounds imgArr
        imgArr' = amap (\(r, g, b, a) -> (pack . P.map fromIntegral) [r, g, b]) imgArr
        imgData = P.foldl (\acc new -> BS.concat [acc, new]) BS.empty imgArr'

    in do 
            fHandle <- openFile name ReadWriteMode

            hPutStrLn fHandle $ "P6\n" ++ show width ++ " " ++ show height ++  " " ++ "255"
            BS.hPut fHandle imgData

            hClose fHandle

