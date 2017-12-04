module Main where


--import System.Random.Shuffle
import Shuffle (shuffleM)
import Control.Monad (liftM)
import Data.Time.Clock
import Data.Vector
import Prelude hiding (head)

import GA
import TSP

generationNum = 100

inputPath :: IO [Coord]
inputPath = do
  contents <- getContents
  return $ toCoord . words <$> lines contents
    where
      toCoord :: [String] -> Coord
      toCoord [s1, s2] = Coord (read s1, read s2)
      toCoord _ = error "it is illegal file format!"

main :: IO ()
main = do
  start <- getCurrentTime

  coords <- inputPath

  paths <- replicateM poolSize $ Path <$> shuffleM (fromList coords)
  elite <- geneticLoop generationNum paths (head paths)

  print $ totalCost elite

  end <- getCurrentTime
  print $ diffUTCTime end start
