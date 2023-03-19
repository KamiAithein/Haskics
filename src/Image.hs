module Image where

import Prelude as P
import System.IO
import Numeric
import Data.ByteString as BS

import Control.Monad.ST
import Control.Monad

import Data.Array.IArray
import Data.Array.ST

type RawPixel = (Int, Int, Int, Int)    -- r g b a
type RawBounds = (Int, Int)             -- y, x
type RawImage s = STArray s RawBounds RawPixel

newtype MImage a = MImage a

instance Functor (MImage) where
    fmap f (MImage a) = MImage $ f a

instance Applicative (MImage) where
    (MImage f) <*> (MImage a) = MImage $ f a
    pure a = MImage a

instance Monad (MImage) where
    (MImage a) >>= f = f a
    return = pure

type Image s = ST s (MImage (RawImage s))

-- -- map each pixel, using f to map
map2 :: (a -> b) -> [[a]] ->  [[b]]
map2 f arr =  P.map (\row -> P.map f row) arr

createImage :: Int -> Int -> Image s
createImage width height = return $ return $ createRawImage width height

createRawImage :: Int -> Int -> RawImage s
createRawImage width height = thaw $ array ((1,1), (width,height)) $ [((i, j), (0, 0, 0, 0)) | i <- [1..width], j <- [1..height]] :: STArray s RawBounds RawPixel


saveImage :: Image RealWorld -> String -> IO ()
saveImage img name = do
    (MImage img') <- stToIO img
    saveRawImage img' name
    

saveRawImage :: RawImage s -> String -> IO ()
saveRawImage imgArr name =
    let ((_, _), (width, height)) = bounds imgArr
        imgArr' = amap (\(r, g, b, a) -> (pack . P.map fromIntegral) [r, g, b]) imgArr
        imgData = P.foldl (\acc new -> BS.concat [acc, new]) BS.empty imgArr'

    in do 
            fHandle <- openFile name ReadWriteMode

            hPutStrLn fHandle $ "P6\n" ++ show width ++ " " ++ show height ++  " " ++ "255"
            BS.hPut fHandle imgData

            hClose fHandle

