module Main where

import System.Random

main = do
  g <- getStdGen
  print $ take 20 (randoms g :: [Double])
