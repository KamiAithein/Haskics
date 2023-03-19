module Main (main) where

import Data.Array
import Data.Array.ST

import Control.Monad
import Control.Monad.ST

data Image s = Image 
    { imgImg :: STArray s Int Int
    }

createImage :: Int -> ST s (Image s)
createImage width = do
    arr <- thaw $ array (1, width) [(i, 0) | i <- [1..width]] :: ST s (STArray s Int Int)
    return Image { imgImg = arr }

setAllOnes :: Image s -> ST s ()
setAllOnes img = do
    (_, hi) <- getBounds $ imgImg img
    forM_ [1..hi] (\i -> writeArray (imgImg img) i 1) 

setHalfTwos :: Image s -> ST s ()
setHalfTwos img = do
    (_, hi) <- getBounds $ imgImg img
    forM_ [1..hi `div` 2] (\i -> writeArray (imgImg img) i 2)


main :: IO ()
main = 
    let arr = runSTArray $ do 
            img <- createImage 10
            setAllOnes img
            setHalfTwos img
            return $ imgImg img
    in do
            forM_ (arr :: Array Int Int) (\val -> print $ val)
