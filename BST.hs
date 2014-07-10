module BST
       where

import BinTree

type BST a = BinTree a

bst_as_bin_tree :: BST a -> BinTree a
bst_as_bin_tree t = t

bst_show :: Show a => BST a -> [[[Char]]]
bst_show  = bin_tree_show  . bst_as_bin_tree
count_bst = count_bin_tree . bst_as_bin_tree
bst_depth = bin_tree_depth . bst_as_bin_tree

rotate_bst_left EmptyBinTree                                                           = EmptyBinTree
rotate_bst_left t@( BinTree vt EmptyBinTree               EmptyBinTree               ) = t
rotate_bst_left t@( BinTree vt lt                         EmptyBinTree               ) = t
rotate_bst_left t@( BinTree vt EmptyBinTree               rt                         ) = BinTree ( value rt ) t EmptyBinTree
rotate_bst_left t@( BinTree vt lt@( BinTree vlt llt rlt ) rt@( BinTree vrt lrt rrt ) ) = BinTree vrt ( BinTree vt lt lrt ) rrt

rotate_bst_right EmptyBinTree                                                           = EmptyBinTree
rotate_bst_right t@( BinTree vt EmptyBinTree               EmptyBinTree               ) = t
rotate_bst_right t@( BinTree vt lt                         EmptyBinTree               ) = BinTree ( value lt ) EmptyBinTree t
rotate_bst_right t@( BinTree vt EmptyBinTree               rt                         ) = t
rotate_bst_right t@( BinTree vt lt@( BinTree vlt llt rlt ) rt@( BinTree vrt lrt rrt ) ) = BinTree vlt llt (BinTree vt rlt rt)

add_to_bst :: Ord a => BST a -> a -> BST a
add_to_bst EmptyBinTree                                                            nv             = BinTree nv EmptyBinTree           EmptyBinTree
add_to_bst t@( BinTree vt EmptyBinTree               EmptyBinTree )                nv | nv > vt   = BinTree nv ( init_bin_tree vt  )  EmptyBinTree
                                                                                      | otherwise = BinTree vt ( init_bin_tree nv )   EmptyBinTree
                                                         
add_to_bst t@( BinTree vt lt@( BinTree vlt llt rlt ) EmptyBinTree )                nv | nv > vt   = BinTree vt  lt                    ( init_bin_tree nv )  -- nv > v >= l
                                                                                      | nv > vlt  = BinTree nv  lt                    ( init_bin_tree vt )  -- v >= nv > l
                                                                                      | otherwise = BinTree vlt ( add_to_bst llt nv ) ( add_to_bst rlt vt ) -- v >= l >= nv

add_to_bst t@( BinTree vt EmptyBinTree               rt@( BinTree vrt lrt rrt ) )  nv | nv > vrt  = BinTree vrt ( add_to_bst lrt vt ) ( add_to_bst rrt nv ) -- nv > r  > v
                                                                                      | nv > vt   = BinTree nv  ( init_bin_tree vt )  rt                    -- r  > nv > v
                                                                                      | otherwise = BinTree vt  ( init_bin_tree nv )  rt                    -- r  > v  > nv

add_to_bst t@( BinTree vt lt@( BinTree vlt llt rlt ) rt @( BinTree vrt lrt rrt ) ) nv | nv > vrt  = BinTree vt lt                     ( add_to_bst rt nv )  -- nv  > r  >  v  >= l
                                                                                      | nv > vt   = BinTree vt lt                     ( add_to_bst rt nv )  -- r  >= nv >  v  >= l
                                                                                      | nv > vlt  = BinTree vt ( add_to_bst lt nv )   rt                    -- r  >  v  >= nv >  l
                                                                                      | otherwise = BinTree vt ( add_to_bst lt nv )   rt                    -- r  >  v  >= l  >= nv

make_bst :: Ord a => [ a ] -> BST a
make_bst = foldl add_to_bst EmptyBinTree

search_bst v EmptyBinTree = EmptyBinTree
search_bst v t@( BinTree vt lt rt ) | v >  vt   = search_bst v rt
                                    | v == vt   = t
                                    | otherwise = search_bst v lt

