module Geom where

import System.IO
import Data.List
import Data.String

-- simulated 3D geometry
type Vertex = (Int, Int, Int)
type Face = [Int]
data Geom = Geom 
    { gVertices :: [Vertex]
    , gFaces    :: [Face] 
    }
    deriving (Show)

geomFromFile :: String -> IO Geom
geomFromFile fName = do
    fHandle <- openFile fName ReadMode

    raw <- hGetContents fHandle -- closes handle
    let rawData = (map words . lines) $ raw
    let vert = map (\[v, x, y, z] -> (read x :: Int, read y :: Int, read z :: Int)) $ filter (\(label:rest) -> label == "v") rawData
    let face = map (\line -> map (\float -> round (read float :: Float)) line) $ map (\(f:rest) -> rest) $ filter (\(label:rest) -> label == "f") rawData

    let geom = Geom { gVertices = vert, gFaces = face }

    return geom 

