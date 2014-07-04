module BST
       where

import BinTree

type BST a = BinTree a

bst_as_bin_tree :: BST a -> BinTree a
bst_as_bin_tree t = t

bst_show :: Show a => BST a -> [[[Char]]]
bst_show = bin_tree_show . bst_as_bin_tree

count_bst = count_bin_tree . bst_as_bin_tree


rotate_bst_left EmptyBinTree = EmptyBinTree
rotate_bst_left t            = rotate_bst_left' t ( value t ) ( left t ) ( right t )

rotate_bst_left' t vt EmptyBinTree EmptyBinTree = t
rotate_bst_left' t vt lt           EmptyBinTree = t
rotate_bst_left' t vt EmptyBinTree rt           = BinTree ( value rt ) t EmptyBinTree
rotate_bst_left' t vt lt           rt           = rotate_bst_left'' t vt  lt rt ( value lt ) ( left lt ) ( right lt ) ( value rt ) ( left rt ) ( right rt )

rotate_bst_left'' t vt lt rt vlt llt rlt vrt lrt rrt = let tnode  = BinTree vt lt lrt
                                                       in BinTree vrt tnode rrt

rotate_bst_right EmptyBinTree = EmptyBinTree
rotate_bst_right t            = rotate_bst_right' t ( value t ) ( left t ) ( right t )

rotate_bst_right' t vt EmptyBinTree EmptyBinTree = t
rotate_bst_right' t vt lt           EmptyBinTree = BinTree ( value lt ) EmptyBinTree t
rotate_bst_right' t vt EmptyBinTree rt           = t
rotate_bst_right' t vt lt           rt           = rotate_bst_right'' t vt  lt rt ( value lt ) ( left lt ) ( right lt ) ( value rt ) ( left rt ) ( right rt )

rotate_bst_right'' t vt lt rt vlt llt rlt vrt lrt rrt = let tnode  = BinTree vt rlt rt
                                                        in BinTree vlt llt tnode


add_to_bst :: Ord a => BST a -> a -> BST a
add_to_bst EmptyBinTree nv = BinTree nv EmptyBinTree EmptyBinTree
add_to_bst t            nv = add_to_bst' t ( value t ) ( left t ) ( right t ) nv

add_to_bst' :: Ord a => BST a -> a -> BST a -> BST a -> a -> BST a
add_to_bst' t vt EmptyBinTree EmptyBinTree nv | nv > vt   = BinTree nv ( init_bin_tree vt  ) EmptyBinTree
                                              | otherwise = BinTree vt  ( init_bin_tree nv ) EmptyBinTree
                                                         
add_to_bst' t vt lt            EmptyBinTree nv | nv > vt           = BinTree vt           lt                            ( init_bin_tree nv )           -- nv > v >= l
                                               | nv > ( value lt ) = BinTree nv           lt                            ( init_bin_tree vt )           -- v >= nv > l
                                               | otherwise         = BinTree ( value lt ) ( add_to_bst ( left lt ) nv ) ( add_to_bst ( right lt ) vt ) -- v >= l >= nv

  
add_to_bst' t vt EmptyBinTree rt            nv | nv > ( value rt ) = BinTree ( value rt ) ( add_to_bst ( left rt ) vt ) ( add_to_bst ( right rt ) nv ) -- nv > r  > v
                                               | nv > vt           = BinTree nv           ( init_bin_tree vt )          rt                             -- r  > nv > v
                                               | otherwise         = BinTree vt           ( init_bin_tree nv )          rt                             -- r  > v  > nv


add_to_bst' t vt lt rt nv | nv > ( value rt ) = BinTree vt lt                      ( add_to_bst rt nv ) -- nv  > r  >  v  >= l
                          | nv > vt           = BinTree vt lt                      ( add_to_bst rt nv ) -- r  >= nv >  v  >= l
                          | nv > ( value lt ) = BinTree vt ( add_to_bst lt nv )    rt                   -- r  >  v  >= nv >  l
                          | otherwise         = BinTree vt ( add_to_bst lt nv )    rt                   -- r  >  v  >= l  >= nv


-- For symmetry we can have some rotations
-- Doesn't help
-- add_to_bst' t vt lt rt nv | nv > ( value rt ) =  rotate_bst_left ( BinTree vt lt                      ( add_to_bst rt nv ) ) -- nv  > r  >  v  >= l
--                           | nv > vt           =                    BinTree vt lt                      ( add_to_bst rt nv )   -- r  >= nv >  v  >= l
--                           | nv > ( value lt ) =                    BinTree vt ( add_to_bst lt nv )    rt                     -- r  >  v  >= nv >  l
--                           | otherwise         = rotate_bst_right ( BinTree vt ( add_to_bst lt nv )    rt                   ) -- r  >  v  >= l  >= nv

make_bst l = foldl add_to_bst EmptyBinTree l




search_bst v EmptyBinTree = EmptyBinTree
search_bst v t | v >  ( value t ) = search_bst v ( right t )
               | v == ( value t ) = t
               | otherwise        = search_bst v ( left t )














-- Simple insert --- v always remains on top
-- add_to_bst'' t vt lt            rt            nv | nv > ( value rt ) = BinTree vt           lt                    ( add_to_bst rt nv ) -- nv  > r  >  v  >= l
--                                                  | nv > vt           = BinTree vt           lt                    ( add_to_bst rt nv ) -- r  >= nv >  v  >= l
--                                                  | nv > ( value lt ) = BinTree vt           ( add_to_bst lt nv )  rt                   -- r  >  v  >= nv >  l
--                                                  | otherwise         = BinTree vt           ( add_to_bst lt nv )  rt                   -- r  >  v  >= l  >= nv
