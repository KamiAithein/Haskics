{-# LANGUAGE NamedFieldPuns #-}

module Image where

import Prelude as P
import System.IO
import Numeric
import Data.ByteString as BS

import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.ST

type RawPixel = (Int, Int, Int, Int)    -- r g b a
type RawBounds = (Int, Int)             -- y, x
type RawImage s = STArray s RawBounds RawPixel

data Image s = Image
    {   imageImg :: RawImage s
    }

createImage :: Int -> Int -> ST s (Image s)
createImage width height = do
    arr <- thaw $ array ((1, 1), (width, height)) [((i, j), (0, 0, 0, 0)) | i <- [1..width], j <- [1..width]] :: ST s (RawImage s)
    return Image { imageImg = arr }  

saveImage :: String -> Image s -> ST s (IO ())
saveImage name Image { imageImg } = do
    ((_,_), (width, height)) <- getBounds imageImg
    elems <- getElems imageImg
    let elemsPack = P.map (\(r, g, b, a) -> (pack . P.map fromIntegral) [r, g, b]) elems
    let elemsPack2 = BS.concat elemsPack
    let imgData = elemsPack2

    return $ do
            fHandle <- openFile name ReadWriteMode

            hPutStrLn fHandle $ "P6\n" ++ show width ++ " " ++ show height ++  " " ++ "255"
            BS.hPut fHandle imgData

            hClose fHandle

