module Main
       where

import System.Random
import BST
import BinTree


main = do
  g <- getStdGen
  let nums = take 20 ( randomRs (1::Int, 1000::Int ) g )
      h = bst_make nums
      in print ( bst_show h )
  putStrLn ""
  let nums = take 20 ( randomRs (1::Int, 1000::Int ) g )
      h = bst_make nums
      in print ( bst_count h )
  putStrLn ""
  let nums = take 200 ( randomRs (1::Int, 1000::Int ) g )
      h = bst_make nums
      in print ( bin_tree_depth ( bst_as_bin_tree h ) )
  putStrLn ""
  let nums = [ 1 .. 200 ]
      h = bst_make nums
      in print ( bin_tree_depth ( bst_as_bin_tree h ) )
  putStrLn ""
  let nums = reverse [ 1 .. 200 ]
      h = bst_make nums
      in print ( bin_tree_depth ( bst_as_bin_tree h ) )
  let nums = take 200 ( randomRs (1::Int, 100::Int ) g )
      h = bst_make nums
      in print ( bin_tree_show ( bst_as_bin_tree ( bst_search 20 h ) ) )
