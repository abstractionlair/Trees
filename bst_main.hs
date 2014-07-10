module Main
       where

import System.Random
import BST
import BinTree


main = do
  g <- getStdGen
  let nums = take 20 ( randomRs (1::Int, 1000::Int ) g )
      h = make_bst nums
      in print ( bst_show h )
  putStrLn ""
  let nums = take 20 ( randomRs (1::Int, 1000::Int ) g )
      h = make_bst nums
      in print ( count_bst h )
  putStrLn ""
  let nums = take 200 ( randomRs (1::Int, 1000::Int ) g )
      h = make_bst nums
      in print ( bin_tree_depth ( bst_as_bin_tree h ) )
  putStrLn ""
  let nums = [ 1 .. 200 ]
      h = make_bst nums
      in print ( bin_tree_depth ( bst_as_bin_tree h ) )
  putStrLn ""
  let nums = reverse [ 1 .. 200 ]
      h = make_bst nums
      in print ( bin_tree_depth ( bst_as_bin_tree h ) )
  let nums = take 200 ( randomRs (1::Int, 100::Int ) g )
      h = make_bst nums
      in print ( bin_tree_show ( bst_as_bin_tree ( search_bst 20 h ) ) )
