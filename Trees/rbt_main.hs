module Main
       where

import System.Random
import RBT
import BST
import BinTree


main = do
  g <- getStdGen
  let nums = take 20 ( randomRs (1::Int, 1000::Int ) g )
      h = make_rbt nums
      in print ( bst_show ( rbt_as_bst h) )
  putStrLn ""
  let nums = take 20 ( randomRs (1::Int, 1000::Int ) g )
      h = make_rbt nums
      in print ( bst_count ( rbt_as_bst h ) )
  putStrLn ""
  let nums = take 200 ( randomRs (1::Int, 1000::Int ) g )
      h = make_rbt nums
      in print ( bin_tree_depth ( rbt_as_bin_tree h ) )
  putStrLn ""
  let nums = take 200 ( randomRs (1::Int, 1000::Int ) g )
      h = make_rbt nums
      in print ( rbt_show ( rbt_as_bin_tree h ) )
  putStrLn ""
  let nums = [ 1 .. 200 ]
      h = make_rbt nums
      in print ( bin_tree_depth ( rbt_as_bin_tree h ) )
  putStrLn ""
  let nums = reverse [ 1 .. 200 ]
      h = make_rbt nums
      in print ( bin_tree_depth ( rbt_as_bin_tree h ) )
  let nums = take 200 ( randomRs (1::Int, 100::Int ) g )
      h = make_rbt nums
      in print ( bin_tree_show ( rbt_as_bin_tree ( bst_search ( RBItem Black 20 ) (rbt_as_bst h ) ) ) )
