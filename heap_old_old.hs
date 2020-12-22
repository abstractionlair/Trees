{-
        Heap rules
        1) value of a node is greater than or equal to all lower values
        2) and add to that
           a) value of left is greater than or equal to right
           b) any value is greater than or equal to empty

        Note: not forcing any balance
-}
   

data Heap a
    = EmptyHeap
    | HeapNode { value :: a, left :: Heap a, right :: Heap a } deriving Show

add_to_heap :: Ord a => Heap a    -> a -> Heap a
add_to_heap             EmptyHeap    v  = HeapNode v EmptyHeap EmptyHeap
add_to_heap             h            v  = add_to_heap0 (value h) (left h) (right h) v


add_to_heap0 :: Ord a => a -> Heap a    -> Heap a    -> a                   -> Heap a

add_to_heap0             v    EmptyHeap    EmptyHeap    nv | nv > v          = HeapNode nv ( HeapNode v  EmptyHeap EmptyHeap ) EmptyHeap                           -- nv >  v
                                                           | otherwise       = HeapNode v  ( HeapNode nv EmptyHeap EmptyHeap ) EmptyHeap                           -- v  >= nv

add_to_heap0             v    l            EmptyHeap    nv | nv >  v         = HeapNode nv ( HeapNode v  EmptyHeap EmptyHeap ) l                                   -- nv >  v         >= (value l)
                                                           | nv >  (value l) = HeapNode v  ( HeapNode nv EmptyHeap EmptyHeap ) l                                   -- v  >= nv        >= (value l)
                                                           | otherwise       = HeapNode v  l                                   ( HeapNode nv EmptyHeap EmptyHeap ) -- v  >= (value l) >= nv

add_to_heap0             v    l            r            nv | nv >  v         = HeapNode nv ( add_to_heap r v )                 l                                   -- nv >  v         >= (value l) >= (value r)
                                                           | nv >  (value l) = HeapNode v  ( add_to_heap r nv )                l                                   -- v  >= nv        >= (value l) >= (value r)
                                                           | nv >  (value r) = HeapNode v  l                                   ( add_to_heap r nv )                -- v  >= (value l) >= nv        >= (value r)
                                                           | otherwise       = HeapNode v  ( add_to_heap l nv )                r                                   -- v  >= (value l) >= (value r) >= nv

make_heap l = foldl add_to_heap EmptyHeap l



pop_heap EmptyHeap = EmptyHeap
pop_heap h         = pop_heap0 (left h) (right h)

pop_heap0 EmptyHeap EmptyHeap     = EmptyHeap
pop_heap0 l         EmptyHeap     = l
pop_heap0 l        r              = pop_heap00 l r pl
                                    where pl  = pop_heap l

pop_heap00 l r EmptyHeap                       = HeapNode (value l) r EmptyHeap
pop_heap00 l r pl        | value pl > value r  = HeapNode (value l) pl r
                         | otherwise           = HeapNode (value l) r  pl


unmake_heap0 :: Ord a => [a] -> Heap a -> [a]
unmake_heap0 r EmptyHeap = r
unmake_heap0 r h         = unmake_heap0 ( (value h ):r ) ( pop_heap h )

unmake_heap h = unmake_heap0 [] h
