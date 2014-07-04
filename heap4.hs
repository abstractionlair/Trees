data HeapNode a = EmptyHeapNode
                | HeapNode { parent :: HeapNode a, leftValue :: a, rightValue :: a }

data Heap a = Heap { top :: HeapNode a, bottom :: [ HeapNode a ] }
