module Geom where

import System.IO
import Data.List
import Data.String

import Debug.Trace

-- simulated 3D geometry
type Vertex = (Float, Float, Float)
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
    let vert = map (\[v, x, y, z] -> (read x :: Float, read y :: Float, read z :: Float)) $ filter (\(label:rest) -> label == "v") rawData
    let face = map (\line -> map (\float -> round (read float :: Float)) line) $ map (\(f:rest) -> rest) $ filter (\(label:rest) -> label == "f") rawData

    let geom = trace "read file!" Geom { gVertices = vert, gFaces = face }

    return geom 

