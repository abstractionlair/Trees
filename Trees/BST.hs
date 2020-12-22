module BST
       where

-- Binary Search Trees

import BinTree

type BST a = BinTree a

bst_as_bin_tree :: BST a -> BinTree a
bst_as_bin_tree t = t

bst_show :: Show a => BST a -> [[[Char]]]
bst_show  = bin_tree_show  . bst_as_bin_tree
bst_count = bin_tree_count . bst_as_bin_tree
bst_depth = bin_tree_depth . bst_as_bin_tree

bst_add :: Ord a => BST a -> a -> BST a
bst_add EmptyBinTree                                                            nv             = BinTree nv EmptyBinTree           EmptyBinTree
bst_add t@( BinTree vt EmptyBinTree               EmptyBinTree )                nv | nv > vt   = BinTree nv ( bin_tree_init vt  )  EmptyBinTree
                                                                                   | otherwise = BinTree vt ( bin_tree_init nv )   EmptyBinTree
                                                         
bst_add t@( BinTree vt lt@( BinTree vlt llt rlt ) EmptyBinTree )                nv | nv > vt   = BinTree vt  lt                    ( bin_tree_init nv )  -- nv > v >= l
                                                                                   | nv > vlt  = BinTree nv  lt                    ( bin_tree_init vt )  -- v >= nv > l
                                                                                   | otherwise = BinTree vlt ( bst_add llt nv ) ( bst_add rlt vt ) -- v >= l >= nv

bst_add t@( BinTree vt EmptyBinTree               rt@( BinTree vrt lrt rrt ) )  nv | nv > vrt  = BinTree vrt ( bst_add lrt vt ) ( bst_add rrt nv ) -- nv > r  > v
                                                                                   | nv > vt   = BinTree nv  ( bin_tree_init vt )  rt                    -- r  > nv > v
                                                                                   | otherwise = BinTree vt  ( bin_tree_init nv )  rt                    -- r  > v  > nv

bst_add t@( BinTree vt lt@( BinTree vlt llt rlt ) rt @( BinTree vrt lrt rrt ) ) nv | nv > vrt  = BinTree vt lt                     ( bst_add rt nv )  -- nv  > r  >  v  >= l
                                                                                   | nv > vt   = BinTree vt lt                     ( bst_add rt nv )  -- r  >= nv >  v  >= l
                                                                                   | nv > vlt  = BinTree vt ( bst_add lt nv )   rt                    -- r  >  v  >= nv >  l
                                                                                   | otherwise = BinTree vt ( bst_add lt nv )   rt                    -- r  >  v  >= l  >= nv

bst_make :: Ord a => [ a ] -> BST a
bst_make = foldl bst_add EmptyBinTree

bst_search v EmptyBinTree = EmptyBinTree
bst_search v t@( BinTree vt lt rt ) | v >  vt   = bst_search v rt
                                    | v == vt   = t
                                    | otherwise = bst_search v lt

bst_rotate_left EmptyBinTree                                                           = EmptyBinTree
bst_rotate_left t@( BinTree vt EmptyBinTree               EmptyBinTree               ) = t
bst_rotate_left t@( BinTree vt lt                         EmptyBinTree               ) = t
bst_rotate_left t@( BinTree vt EmptyBinTree               rt                         ) = BinTree ( value rt ) t EmptyBinTree
bst_rotate_left t@( BinTree vt lt@( BinTree vlt llt rlt ) rt@( BinTree vrt lrt rrt ) ) = BinTree vrt ( BinTree vt lt lrt ) rrt

bst_rotata_right EmptyBinTree                                                           = EmptyBinTree
bst_rotata_right t@( BinTree vt EmptyBinTree               EmptyBinTree               ) = t
bst_rotata_right t@( BinTree vt lt                         EmptyBinTree               ) = BinTree ( value lt ) EmptyBinTree t
bst_rotata_right t@( BinTree vt EmptyBinTree               rt                         ) = t
bst_rotata_right t@( BinTree vt lt@( BinTree vlt llt rlt ) rt@( BinTree vrt lrt rrt ) ) = BinTree vlt llt (BinTree vt rlt rt)

bst_merge EmptyBinTree         EmptyBinTree                      = EmptyBinTree
bst_merge EmptyBinTree         r                                 = r
bst_merge l                    EmptyBinTree                      = l
bst_merge l@(BinTree vl ll rl) r@(BinTree vr lr rr ) | vl > vr   = BinTree vl ll               (bst_merge rl r)
                                                     | otherwise = BinTree vr (bst_merge l lr) rr

bst_delete EmptyBinTree      ov             = EmptyBinTree
bst_delete t@(BinTree v l r) ov | ov > v    = BinTree v l (bst_delete r ov)
                                | ov == v   = bst_merge l r
                                | otherwise = BinTree v (bst_delete l ov) r
