module AVLTest
       where

import AVL
import BinTree
import Data.List
import Fib
import QuickShuffle
import System.Random
import Test.HUnit

-- Run with : runTestTT tests

tests = TestList [TestLabel "test1" test1
                 ,TestLabel "test2" test2
                 ,TestLabel "test3" test3
                 ,TestLabel "test4" test4
                 ,TestLabel "test5" test5
                 ,TestLabel "test6" test6
                 ,TestLabel "test7" test7
                 ,TestLabel "test8" test8
                 ]
                 

test1 = TestCase (assertEqual "Depth of [1,2,3]" 2 (avl_depth $ avl_make [1,2,3]))

test2 = TestCase (assertBool "Inserted values are there after insert" allInAfterInsert)

test3 = TestCase (assertBool "Not inserted values are not there notInsertedNotIn" notInsertedNotIn )

test4 = TestCase (assertBool "Some inserted values are still there after deleting others" someInAfterDelete )

test5 = TestCase (assertBool "Deleted values are not there after deleting " someNotInAfterDelete )

test6 = TestCase (assertBool "Not inserted values are still not there after deleting others" someMoreNotInAfterDelete )

test7 = TestCase (assertBool "Valid depth after inserts" validDepthAfterInsert)

test8 = TestCase (assertBool "Valid depth after deletes" validDepthAfterDelete)




test_sample seed inL inR inN outL outR outN = let
                                                 ig1      = mkStdGen seed
                                                 (g1,ig2) = split ig1
                                                 (g2,ig3) = split ig2
                                                 (g3,ig4) = split ig3
                                                 (g4,ig5) = split ig3
                                                 numsIn   = nub $ take inN $ (randomRs (inL,inR) g1)                                -- These are in the tree.
                                                 sNumsIn  = shuffle g3 numsIn                                                       -- So are these, 
                                                 (sNumsIn1,sNumsIn2,g5) = rpartition g4 sNumsIn                                     -- but shuffled and partitioned.
                                                 numsOut  = filter (\x -> notElem x numsIn) $ take outN $ (randomRs (outL,outR) g2) -- These are not in the tree.
                                                 avl      = avl_make numsIn
                                              in (avl, numsIn, sNumsIn1, sNumsIn2, numsOut)

multiAnd = foldl (&&) True
                                                 
allIn t numsIn = multiAnd $ map (\e -> avl_elem e t) numsIn

noneIn t numsOut = multiAnd $ map (\e -> not (avl_elem e t)) numsOut



allInAfterInsert = let (avl, numsIn, sNumsIn1, sNumsIn2, numsOut) = test_sample 2390 (100::Int) 200 100 0 400 100
                   in allIn avl (sNumsIn1 ++ sNumsIn2)

notInsertedNotIn = let (avl, numsIn, sNumsIn1, sNumsIn2, numsOut) = test_sample 2391 (100::Int) 200 100 0 400 100
                   in noneIn avl numsOut

someInAfterDelete = let (avl, numsIn, sNumsIn1, sNumsIn2, numsOut) = test_sample 2392 (100::Int) 200 100 0 400 100
                        navl = foldl avl_delete avl sNumsIn1
                    in allIn navl sNumsIn2

someNotInAfterDelete = let (avl, numsIn, sNumsIn1, sNumsIn2, numsOut) = test_sample 2393 (100::Int) 200 100 0 400 100
                           navl = foldl avl_delete avl sNumsIn1
                       in noneIn navl sNumsIn1

someMoreNotInAfterDelete = let (avl, numsIn, sNumsIn1, sNumsIn2, numsOut) = test_sample 2394 (100::Int) 200 100 0 400 100
                               navl = foldl avl_delete avl sNumsIn1
                           in noneIn navl numsOut



validDepth EmptyBinTree = True
validDepth t@(BinTree v l r) = (depth v == bin_tree_depth t) && (validDepth l) && (validDepth r)

validDepthAfterInsert = let (avl, numsIn, sNumsIn1, sNumsIn2, numsOut) = test_sample 2395 (100::Int) 200 100 0 400 100
                        in validDepth avl

validDepthAfterDelete = let (avl, numsIn, sNumsIn1, sNumsIn2, numsOut) = test_sample 2395 (100::Int) 200 100 0 400 100
                            navl = foldl avl_delete avl sNumsIn1
                        in validDepth navl

worstHeight n = log ( (sqrt 5) * (n+2.0)) / (log phi)
                where phi = (1.0 + sqrt(5))/2.0

-- TODO: get comfy with Haskel's list comprehensions
goodHeights = let (g1,g2)      = split $ mkStdGen 192837465
                  seeds        = take 20 $ (randoms g1) :: [Int]
                  sizes        = take 20 $ (randomRs (1::Int,10000) g2)
                  bounds       = map worstHeight (map fromIntegral sizes)
                  aTree s d    = let (avl, numsIn, sNumsIn1, sNumsIn2, numsOut) = test_sample s (0::Int) 1000000 d 0 1 1
                                 in avl
                  depths       = map (\(s,d) -> avl_depth $ aTree s d) (zip seeds sizes)
                  oks          = map (\(d,b) -> (fromIntegral d) <= b) (zip depths bounds)
              in multiAnd oks
                  
