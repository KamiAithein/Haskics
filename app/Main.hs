module Main (main) where

import Data.Array
import Data.Array.ST

import Control.Monad
import Control.Monad.ST

data Image s = Image 
    { imgImg :: STArray s Int Int
    }

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
            arr <- thaw $ array (1, 10) [(i, 0) | i <- [1..10]] :: ST s (STArray s Int Int)
            let img = Image {imgImg=arr}
            setAllOnes img
            setHalfTwos img
            return arr
    in do
            forM_ arr (\val -> putStrLn $ show val)
