module BST
       where

import BinTree

type BST a = BinTree a

add_to_bst :: Ord a => BST a -> a -> BST a
add_to_bst EmptyBinTree nv = BinTree nv EmptyBinTree EmptyBinTree
add_to_bst h            nv = add_to_bst' ( value h ) ( left h ) ( right h ) nv

add_to_bst' :: Ord a => a -> BST a -> BST a -> a -> BST a

add_to_bst' v EmptyBinTree EmptyBinTree nv | nv > v    = BinTree nv ( init_bin_tree v  ) EmptyBinTree
                                           | otherwise = BinTree v  ( init_bin_tree nv ) EmptyBinTree
                                                         
add_to_bst' v l            EmptyBinTree nv | nv > v           = BinTree v           l                            ( init_bin_tree nv )         -- nv > v >= l
                                           | nv > ( value l ) = BinTree nv          l                            ( init_bin_tree v )          -- v >= nv > l
                                           | otherwise        = BinTree ( value l ) ( add_to_bst ( left l ) nv ) ( add_to_bst ( right l ) v ) -- v >= l >= nv

  
add_to_bst' v EmptyBinTree r            nv | nv > ( value r ) = BinTree ( value r ) ( add_to_bst ( left r ) v ) ( add_to_bst ( right r ) nv ) -- nv > r  > v
                                           | nv > v           = BinTree nv          ( init_bin_tree v )         r                             -- r  > nv > v
                                           | otherwise        = BinTree v           ( init_bin_tree nv )        r                             -- r  > v  > nv 


-- For symmetry we have
--  left rotation
--  add right
--  add left
--  right rotation
add_to_bst' v l            r            nv | nv > ( value r ) = BinTree ( value r ) ( add_to_bst l v )   ( init_bin_tree nv ) -- nv  > r  >  v  >= l
                                           | nv > v           = BinTree v           l                    ( add_to_bst r nv )  -- r  >= nv >  v  >= l
                                           | nv > ( value l ) = BinTree v           ( add_to_bst l nv )  r                    -- r  >  v  >= nv >  l
                                           | otherwise        = BinTree ( value l ) ( init_bin_tree nv ) ( add_to_bst r v )   -- r  >  v  >= l  >= nv
                                           
