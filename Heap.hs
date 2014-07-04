module Heap
       where

import BinTree

type Heap a = BinTree a

heap_as_bin_tree :: Heap a -> BinTree a
heap_as_bin_tree h = h

init_heap v = BinTree v EmptyBinTree EmptyBinTree

add_to_heap :: Ord a  => Heap a       -> a  -> Heap a
add_to_heap              EmptyBinTree    nv  = BinTree nv EmptyBinTree EmptyBinTree
add_to_heap              h               nv  = add_to_heap' (value h) (left h) (right h) nv

add_to_heap' :: Ord a => a -> Heap a       -> Heap a       -> a                   -> Heap a
add_to_heap'             v    EmptyBinTree    EmptyBinTree    nv | nv > v          = BinTree nv ( init_heap v  )     EmptyBinTree
                                                                 | otherwise       = BinTree v  ( init_heap nv )     EmptyBinTree
add_to_heap'             v    l               EmptyBinTree    nv | nv >  v         = BinTree nv ( init_heap v  )     l
                                                                 | otherwise       = BinTree v  ( init_heap nv )     l
add_to_heap'             v    EmptyBinTree    r               nv | nv >  v         = BinTree nv r                    ( init_heap v )
                                                                 | otherwise       = BinTree v  r                    ( init_heap nv )
add_to_heap'             v    l               r               nv | nv >  v         = BinTree nv ( add_to_heap r v  ) l                -- Swap left and right for balance
                                                                 | otherwise       = BinTree v  ( add_to_heap r nv ) l

make_heap l = foldl add_to_heap EmptyBinTree l

pop_heap EmptyBinTree = EmptyBinTree
pop_heap h            = pop_heap' (left h) (right h)

pop_heap' EmptyBinTree EmptyBinTree                         = EmptyBinTree
pop_heap' l            EmptyBinTree                         = l
pop_heap' EmptyBinTree r                                    = r
pop_heap' l            r            | (value l) > (value r) = BinTree (value l) (pop_heap l) r
                                    | otherwise             = BinTree (value r) l            (pop_heap r)

unmake_heap h = unmake_heap' [] h

unmake_heap' :: Ord a => [a] -> Heap a -> [a]
unmake_heap' r EmptyBinTree = r
unmake_heap' r h            = unmake_heap' ( (value h ):r ) ( pop_heap h )
