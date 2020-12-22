module Main
       where

import System.IO
import System.Random

main = do
  num <- randomRIO (1::Int, 100 )
  putStrLn ( show num )
  g <- getStdGen
  print $ take 5 (randoms g :: [Double])
  print $ take 5 ( randomRs ('a', 'z') g)
  let nums = take 5 ( randomRs (1::Int, 100::Int ) g )
      in print nums
  
