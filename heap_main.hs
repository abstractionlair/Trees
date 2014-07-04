module Main
       where

import System.Random
import Heap
import BinTree

gen nums = show ( make_heap nums )

main = do
  g <- getStdGen
  --putStrLn ( gen ( take 20 ( randoms g :: [Double] ) ) )
  let nums = take 20 ( randomRs (1::Int, 1000::Int ) g )
      h = make_heap nums
      in print ( bin_tree_show ( heap_as_bin_tree h ) )
  putStrLn ""
  let nums = take 20 ( randomRs (1::Int, 1000::Int ) g )
      h = make_heap nums
      in print ( unmake_heap h )
  putStrLn ""
  let nums = take 20 ( randomRs (1::Int, 1000::Int ) g )
      h = make_heap nums
      in print ( count_bin_tree ( heap_as_bin_tree h ) )
         
  


