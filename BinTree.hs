module BinTree
       where

-- Binary Trees


-- Every tutorial defines this as Tree, but it is specifically binary.
-- It would be nice to abbreviate to BTree, but that imples something else.
data BinTree a = EmptyBinTree
               | BinTree { value :: a, left :: BinTree a, right :: BinTree a }
                 deriving Show

bin_tree_init v = BinTree v EmptyBinTree EmptyBinTree

bin_tree_next_row :: [ BinTree a ] -> [ BinTree a ]
bin_tree_next_row    []             = []
bin_tree_next_row    (h:hs)         = reverse ( bin_tree_next_row' [] (h:hs) )

bin_tree_next_row' :: [ BinTree a] -> [ BinTree a ]     -> [ BinTree a ]
bin_tree_next_row'    res             []                 = res
bin_tree_next_row'    res             (EmptyBinTree:hs)  = bin_tree_next_row' res hs
bin_tree_next_row'    res             (h:hs)             = bin_tree_next_row' ( (right h):(left h):res ) hs

bin_tree_rows h = reverse ( bin_tree_rows' [] [h] )

bin_tree_rows' :: [ [ BinTree a] ] -> [ BinTree a ] -> [ [ BinTree a ] ]
bin_tree_rows'    res                 []             = res
bin_tree_rows'    res                 (h:hs)         = bin_tree_rows' ( (h:hs):res ) ( bin_tree_next_row (h:hs) )

bin_tree_foldl_by_row :: ( b -> BinTree a -> b ) -> b    -> BinTree a -> b
bin_tree_foldl_by_row    fn                         init    t          = bin_tree_foldl_by_row' fn init [t]

bin_tree_foldl_by_row' :: ( b -> BinTree a -> b ) -> b    -> [ BinTree a ] -> b
bin_tree_foldl_by_row'    fn                         init    []             = init
bin_tree_foldl_by_row'    fn                         init    (t:ts)         = bin_tree_foldl_by_row' fn newVal nextRow
                                                                              where newVal  = foldl fn init (t:ts)
                                                                                    nextRow = bin_tree_next_row (t:ts)

bin_tree_show h = map ( map bin_tree_show' ) (bin_tree_rows h)

bin_tree_show' EmptyBinTree = "E"
bin_tree_show' h            = show ( value h )

bin_tree_weight :: BinTree a -> Int
bin_tree_weight EmptyBinTree = 0
bin_tree_weight t            = 1

bin_tree_count t = bin_tree_foldl_by_row countOne 0 t
                   where countOne v t = v + bin_tree_weight t


bin_tree_depth :: (Num a, Ord a) => BinTree a1 -> a
bin_tree_depth EmptyBinTree                     = 0
bin_tree_depth t@( BinTree vt EmptyBinTree EmptyBinTree ) = 1
bin_tree_depth t@( BinTree vt lt           EmptyBinTree ) = 1 + bin_tree_depth lt
bin_tree_depth t@( BinTree vt EmptyBinTree rt           ) = 1 + bin_tree_depth rt
bin_tree_depth t@( BinTree vt lt           rt           ) = 1 + max ( bin_tree_depth lt ) ( bin_tree_depth rt )
