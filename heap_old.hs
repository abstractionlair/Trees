data Heap a
    = EmptyHeap
    | HeapNode { value :: a, left :: Heap a, right :: Heap a } deriving Show

add_to_heap :: Ord a => Heap a    -> a -> Heap a
add_to_heap             EmptyHeap    v  = HeapNode v EmptyHeap EmptyHeap
add_to_heap             h            v  = add_to_heap0 (value h) (left h) (right h) v


add_to_heap0 :: Ord a => a -> Heap a    -> Heap a    -> a                   -> Heap a

add_to_heap0             v    EmptyHeap    EmptyHeap    nv | nv > v          = HeapNode nv ( HeapNode v  EmptyHeap EmptyHeap ) EmptyHeap
                                                           | otherwise       = HeapNode v  ( HeapNode nv EmptyHeap EmptyHeap ) EmptyHeap

add_to_heap0             v    l            EmptyHeap    nv | nv >  v         = HeapNode nv ( HeapNode v  EmptyHeap EmptyHeap ) l
                                                           | otherwise       = HeapNode v  ( HeapNode nv EmptyHeap EmptyHeap ) l

add_to_heap0             v    l            r            nv | nv >  v         = HeapNode nv ( add_to_heap r v  )                l -- Swap left and right
                                                           | otherwise       = HeapNode v  ( add_to_heap r nv )                l

make_heap l = foldl add_to_heap EmptyHeap l


pop_heap EmptyHeap = EmptyHeap
pop_heap h         = pop_heap0 (left h) (right h)

pop_heap0 EmptyHeap EmptyHeap                         = EmptyHeap
pop_heap0 l         EmptyHeap                         = l
pop_heap0 l         r         | (value l) > (value r) = HeapNode (value l) (pop_heap l) r
                              | otherwise             = HeapNode (value r) l            (pop_heap r)

unmake_heap0 :: Ord a => [a] -> Heap a -> [a]
unmake_heap0 r EmptyHeap = r
unmake_heap0 r h         = unmake_heap0 ( (value h ):r ) ( pop_heap h )

unmake_heap h = unmake_heap0 [] h
























-- add_to_heap0             v    EmptyHeap    r            nv | nv >  v         = HeapNode nv r ( HeapNode v  EmptyHeap EmptyHeap ) -- Should never get here
--                                                            | otherwise       = HeapNode v  r ( HeapNode nv EmptyHeap EmptyHeap )
-- pop_heap0 EmptyHeap r                                 = r
