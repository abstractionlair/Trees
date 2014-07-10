module BinTree
       where

-- Every tutorial defines this as Tree, but it is specifically binary.
-- It would be nice to abbreviate to BTree, but that imples something else.
data BinTree a = EmptyBinTree
               | BinTree { value :: a, left :: BinTree a, right :: BinTree a }
                 deriving Show

init_bin_tree v = BinTree v EmptyBinTree EmptyBinTree

next_bin_tree_row :: [ BinTree a ] -> [ BinTree a ]
next_bin_tree_row    []             = []
next_bin_tree_row    (h:hs)         = reverse ( next_bin_tree_row' [] (h:hs) )

next_bin_tree_row' :: [ BinTree a] -> [ BinTree a ]     -> [ BinTree a ]
next_bin_tree_row'    res             []                 = res
next_bin_tree_row'    res             (EmptyBinTree:hs)  = next_bin_tree_row' res hs
next_bin_tree_row'    res             (h:hs)             = next_bin_tree_row' ( (right h):(left h):res ) hs

bin_tree_rows h = reverse ( bin_tree_rows' [] [h] )

bin_tree_rows' :: [ [ BinTree a] ] -> [ BinTree a ] -> [ [ BinTree a ] ]
bin_tree_rows'    res                 []             = res
bin_tree_rows'    res                 (h:hs)         = bin_tree_rows' ( (h:hs):res ) ( next_bin_tree_row (h:hs) )

foldl_bin_tree_by_row :: ( b -> BinTree a -> b ) -> b    -> BinTree a -> b
foldl_bin_tree_by_row    fn                         init    t          = foldl_bin_tree_by_row' fn init [t]

foldl_bin_tree_by_row' :: ( b -> BinTree a -> b ) -> b    -> [ BinTree a ] -> b
foldl_bin_tree_by_row'    fn                         init    []             = init
foldl_bin_tree_by_row'    fn                         init    (t:ts)         = foldl_bin_tree_by_row' fn newVal nextRow
                                                                              where newVal  = foldl fn init (t:ts)
                                                                                    nextRow = next_bin_tree_row (t:ts)

bin_tree_show h = map ( map bin_tree_show' ) (bin_tree_rows h)

bin_tree_show' EmptyBinTree = "E"
bin_tree_show' h            = show ( value h )

bin_tree_weight :: BinTree a -> Int
bin_tree_weight EmptyBinTree = 0
bin_tree_weight t            = 1

count_bin_tree t = foldl_bin_tree_by_row countOne 0 t
                   where countOne v t = v + bin_tree_weight t


bin_tree_depth :: (Num a, Ord a) => BinTree a1 -> a
bin_tree_depth EmptyBinTree                     = 0
bin_tree_depth t@( BinTree vt EmptyBinTree EmptyBinTree ) = 1
bin_tree_depth t@( BinTree vt lt           EmptyBinTree ) = 1 + bin_tree_depth lt
bin_tree_depth t@( BinTree vt EmptyBinTree rt           ) = 1 + bin_tree_depth rt
bin_tree_depth t@( BinTree vt lt           rt           ) = 1 + max ( bin_tree_depth lt ) ( bin_tree_depth rt )


  
